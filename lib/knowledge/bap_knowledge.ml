open Core_kernel
open Monads.Std

let trace_leave,trace_enter,trace_reset =
  let points = Hashtbl.create (module String) in
  let leave point =
    let current = Unix.gettimeofday () in
    let time,hits,total = Hashtbl.find_exn points point in
    let time_elapsed = current -. time in
    let hits = hits+1 and total = total +. time_elapsed in
    Hashtbl.set points point (0.0, hits,total);
    let us = Float.to_int64 @@ Float.round (total *. 1e6) in
    Format.eprintf "%16Ld : %16d : %s@\n"
      us hits point in
  let enter point =
    let time = Unix.gettimeofday () in
    Hashtbl.update points point ~f:(function
        | None -> (time,0,0.)
        | Some (_,hits,total) -> (time,hits,total)) in
  let reset () = Hashtbl.clear points in
  leave,enter,reset

module Order = struct
  type partial = LT | EQ | GT | NC
  module type S = sig
    type t
    val order : t -> t -> partial
  end
end

type conflict = exn = ..

module Conflict = struct
  type t = conflict = ..
  let pp = Exn.pp
  let add_printer pr = Caml.Printexc.register_printer pr
  let sexp_of_t = Exn.sexp_of_t
end

module type Id = sig
  type t [@@deriving sexp, hash]
  val zero : t
  val pp : Format.formatter -> t -> unit
  val of_string : string -> t
  include Base.Comparable.S with type t := t
  include Binable.S with type t := t
end

(* static identifiers,
   they should persist, so we will substitute them with uuid later
*)
module type Sid = sig
  include Id
  val incr : t ref -> unit
end

module Uid = Type_equal.Id.Uid

(* temporal identifiers

   Identifiers work like pointers in our runtime, and
   are tagged words. We use 63 bit words, which are
   represented natively as immediate values in 64-bit
   OCaml or as boxed values in 32-bit OCaml.

   We add extra tags:

   Numbers:
     +------------------+---+
     |     payload      | 1 |
     +------------------+---+
      62              1   0

   Atoms:
     +--------------+---+---+
     |     payload  | 1 | 0 |
     +--------------+---+---+
      62              1   0


   Cells:
     +--------------+---+---+
     |     payload  | 0 | 0 |
     +--------------+---+---+
      62              1   0


   So numbers, are tagged with the least significand
   bit set to 1. Not numbers (aka pointers), always
   have the lowest bit set to 0, and are either
   atoms (symbols or objects) with the second bit set,
   and cells, with the second bit cleared. Finally,
   we have the null value, which is represented with
   all zeros, which is neither number, cell, or atom.

   The same arithmetically,
     numbers = {1 + 2*n}  -- all odd numbers
     cells = {4 + 4*n}
     atoms = {6 + 4*n}
     null = 0

   Those four sets are disjoint.


   The chosen representation, allows us to represent
   the following number of elements per class (since
   classes partition values into disjoint sets, objects
   of different classes may have the same values, basically,
   each class has its own heap):

   numbers: 2^62 values (or [-2305843009213693953, 2305843009213693951]
   atoms and cells: 2^61 values.

*)
module type Tid = sig
  include Id
  val null : t
  val first_atom : t
  val first_cell : t
  val next : t -> t

  val is_null : t -> bool
  val is_atom : t -> bool
  val is_cell : t -> bool
  val is_number : t -> bool

  val fits : int -> bool
  val of_int : int -> t
  val fits_int : t -> bool
  val to_int : t -> int

  val untagged : t -> Int63.t
  val atom_of_string : string -> t
  val cell_of_string : string -> t
  val number_of_string : string -> t
end


module Oid : Tid = struct
  include Int63
  let null = zero
  let first_atom = of_int 6
  let first_cell = of_int 4
  let next x = Int63.(x + of_int 4) [@@inline]
  let is_null x = x = zero
  let is_number x = x land one <> zero [@@inline]
  let is_atom x =
    x land of_int 0b01 = zero &&
    x land of_int 0b10 <> zero
  let is_cell x = x land of_int 0b11 = zero
  let to_int63 x = x
  let number_of_string s = (of_string s lsl 1) lor of_int 1
  let cell_of_string s = (of_string s lsl 2)
  let atom_of_string s = (of_string s lsl 2) lor of_int 0b10
  let min_value = min_value asr 1
  let max_value = max_value asr 1
  let fits x =
    let x = of_int x in
    x >= min_value && x <= max_value
  [@@inline]
  let fits_int x =
    x >= of_int Int.min_value &&
    x <= of_int Int.max_value
  [@@inline]
  let of_int x = (of_int x lsl 1) + one [@@inline]
  let to_int x = to_int_trunc (x asr 1) [@@inline]

  (* ordinal of a value in the given category (atoms, cells, numbers) *)
  let untagged x =
    if is_number x then x asr 1 else x asr 2
  [@@inline]

end

module Cid : Sid = Int63
module Pid : Sid = Int63

let user_package = "user"
let keyword_package = "keyword"

type slot_status =
  | Sleep
  | Awoke
  | Ready


module Agent : sig
  type t
  type id
  type reliability
  type signs

  val register :
    ?desc:string ->
    ?package:string ->
    ?reliability:reliability -> string -> t

  val registry : unit -> id list

  val authorative : reliability
  val reliable    : reliability
  val trustworthy : reliability
  val doubtful    : reliability
  val unreliable  : reliability

  val name : id -> string
  val desc : id -> string
  val reliability : id -> reliability

  val set_reliability : id -> reliability -> unit

  val pp : Format.formatter -> t -> unit
  val pp_id : Format.formatter -> id -> unit
  val pp_reliability : Format.formatter -> reliability -> unit

  (* the private interface *)

  val weight : t -> int
  include Base.Comparable.S with type t := t
end = struct
  module Id = String
  type t = Id.t
  type agent = Id.t
  type id = Id.t
  type reliability = A | B | C | D | E [@@deriving sexp]
  type info = {
    name : string;
    desc : string;
    rcls : reliability;
  }
  type signs = Set.M(String).t

  let agents : (agent,info) Hashtbl.t = Hashtbl.create (module String)

  let authorative = A
  let reliable = B
  let trustworthy = C
  let doubtful = D
  let unreliable = E

  let weight = function
    | A -> 16
    | B -> 8
    | C -> 4
    | D -> 2
    | E -> 1

  let register
      ?(desc="no description provided")
      ?(package="user")
      ?(reliability=trustworthy) name =
    let name = sprintf "%s:%s" package name in
    let agent = Caml.Digest.string name in
    if Hashtbl.mem agents agent then
      failwithf "An agent with name `%s' already exists, \
                 please choose another name" name ();
    Hashtbl.add_exn agents agent {
      desc; name; rcls = reliability
    };
    agent

  let registry () = Hashtbl.keys agents

  let info agent = Hashtbl.find_exn agents agent
  let name agent = (info agent).name
  let desc agent = (info agent).desc
  let reliability agent = (info agent).rcls
  let weight agent = weight (reliability agent)

  let set_reliability agent rcls =
    Hashtbl.update agents agent ~f:(function
        | None -> assert false
        | Some agent -> {agent with rcls})

  let pp ppf agent = Format.pp_print_string ppf (name agent)

  let pp_reliability ppf r =
    Sexp.pp ppf (sexp_of_reliability r)

  let pp_id ppf agent =
    let {name; desc; rcls} = info agent in
    Format.fprintf ppf "Class %a %s - %s"
      pp_reliability rcls name desc

  include (String : Base.Comparable.S with type t := t)
end

module Opinions : sig
  type 'a t

  val empty : equal:('a -> 'a -> bool) -> 'a -> 'a t

  val inspect : ('a -> Sexp.t) -> 'a t -> Sexp.t

  val add : Agent.t -> 'a -> 'a t -> 'a t
  val of_list : equal:('a -> 'a -> bool) -> 'a -> (Agent.t,'a) List.Assoc.t -> 'a t
  val choice : 'a t -> 'a

  val compare_votes : 'a t -> 'a t -> int
  val join : 'a t -> 'a t -> 'a t
end = struct
  type 'a opinion = {
    opinion : 'a;
    votes   : Set.M(Agent).t;
  }

  type 'a t = {
    opinions : 'a opinion list;
    equal : 'a -> 'a -> bool;
    empty : 'a;
  }

  let empty ~equal empty = {opinions=[]; equal; empty}

  let inspect sexp_of_opinion {opinions} =
    Sexp.List (List.rev_map opinions ~f:(fun {opinion} ->
        sexp_of_opinion opinion))


  let add_opinion op ({opinions; equal} as ops) =
    let casted,opinions =
      List.fold opinions ~init:(false,[])
        ~f:(fun (casted,opinions) ({opinion; votes} as elt) ->
            if not casted && equal opinion op.opinion
            then true, {
                opinion; votes = Set.union votes op.votes;
              } :: opinions
            else casted,elt :: opinions) in
    if casted
    then {ops with opinions}
    else {
      ops with opinions = op :: opinions
    }

  let add agent opinion = add_opinion {
      opinion;
      votes = Set.singleton (module Agent) agent;
    }

  let join x y =
    List.fold y.opinions ~init:x ~f:(fun ops op -> add_opinion op ops)

  let votes_sum =
    Set.fold ~init:0 ~f:(fun sum agent -> sum + Agent.weight agent)

  let count_votes {opinions} =
    List.fold opinions ~init:0 ~f:(fun sum {votes} ->
        sum + votes_sum votes)

  let compare_votes x y =
    compare (count_votes x) (count_votes y)

  let of_list ~equal bot =
    let init = empty ~equal bot in
    List.fold ~init ~f:(fun opts (agent,data) ->
        add agent data opts)

  let compare x y =
    let w1 = votes_sum x.votes
    and w2 = votes_sum y.votes in
    match Int.compare w1 w2 with
    | 0 -> Set.compare_direct x.votes y.votes
    | n -> n

  let choice {opinions; empty} =
    List.max_elt opinions ~compare |> function
    | Some {opinion} -> opinion
    | None -> empty

end

module Domain = struct
  type 'a t = {
    inspect : 'a -> Sexp.t;
    empty : 'a;
    order : 'a -> 'a -> Order.partial;
    join : 'a -> 'a -> ('a,conflict) result;
    name : string;
  }

  let inspect d = d.inspect
  let empty d = d.empty
  let order d = d.order
  let join d = d.join
  let name d = d.name

  let is_empty {empty; order} x = order empty x = EQ

  exception Join of string * Sexp.t * Sexp.t [@@deriving sexp_of]

  let make_join name inspect order x y =
    match order x y with
    | Order.GT -> Ok x
    | EQ | LT -> Ok y
    | NC -> Error (Join (name, inspect x, inspect y))

  let define ?(inspect=sexp_of_opaque) ?join ~empty ~order name = {
    inspect; empty; order; name;
    join = match join with
      | Some f -> f
      | None -> (make_join name inspect order)
  }

  let partial_of_total order x y : Order.partial = match order x y with
    | 0 -> EQ
    | 1 -> GT
    | _ -> LT

  let total ?inspect ?join ~empty ~order name =
    define ?inspect ?join ~empty name ~order:(partial_of_total order)

  let flat ?inspect ?join ~empty ~equal name =
    define ?inspect ?join ~empty name ~order:(fun x y ->
        match equal empty x, equal empty y with
        | true,true -> EQ
        | true,false -> LT
        | false,true -> GT
        | false,false -> if equal x y then EQ else NC)

  let powerset (type t o)
      (module S : Comparator.S with type t = t
                                and type comparator_witness = o)
      ?(inspect=sexp_of_opaque) name =
    let empty = Set.empty (module S) in
    let order x y : Order.partial =
      if Set.equal x y then EQ else
      if Set.is_subset x y then LT else
      if Set.is_subset y x then GT else NC in
    let join x y = Ok (Set.union x y) in
    let module Inspectable = struct
      include S
      let sexp_of_t = inspect
    end in
    let inspect = [%sexp_of: Base.Set.M(Inspectable).t] in
    define ~inspect ~empty ~order ~join name

  let opinions ?(inspect=sexp_of_opaque) ~empty ~equal name =
    let empty = Opinions.empty ~equal empty in
    let order = partial_of_total (Opinions.compare_votes) in
    let inspect = Opinions.inspect inspect in
    define ~inspect ~empty ~order name

  let mapping (type k o d)
      (module K : Comparator.S with type t = k
                                and type comparator_witness = o)
      ?(inspect=sexp_of_opaque)
      ~equal name =
    let empty = Map.empty (module K) in
    let join x y =
      let module Join = struct exception Conflict of conflict end in
      try Result.return @@ Map.merge x y ~f:(fun ~key:_ -> function
          | `Left v | `Right v -> Some v
          | `Both (x,y) ->
            if equal x y then Some y
            else
              let x = inspect x and y = inspect y in
              let failed = Join (name,x,y) in
              raise (Join.Conflict failed))
      with Join.Conflict err -> Error err in
    let inspect xs =
      Sexp.List (Map.keys xs |> List.map ~f:K.comparator.sexp_of_t) in
    let order x y =
      Map.symmetric_diff x y ~data_equal:equal |>
      Sequence.fold ~init:(0,0,0) ~f:(fun (l,m,r) -> function
          | (_,`Left _)     -> (l+1,m,r)
          | (_,`Right _)    -> (l,m,r+1)
          | (_, `Unequal _) -> (l,m+1,r)) |> function
      | 0,0,0 -> Order.EQ
      | 0,0,_ -> LT
      | _,0,0 -> GT
      | _,_,_ -> NC in
    define ~inspect ~join ~empty ~order name

  let optional ?(inspect=sexp_of_opaque) ?join ~equal name =
    let join_data = match join with
      | Some join -> join
      | None -> fun x y ->
        if equal x y then Ok y
        else Error (Join (name, inspect x, inspect y)) in
    let inspect = sexp_of_option inspect in
    let join x y = match x,y with
      | None,x | x,None -> Ok x
      | Some x, Some y -> match join_data x y with
        | Ok x -> Ok (Some x)
        | Error err -> Error err in
    flat ~inspect ~join ~empty:None ~equal:(Option.equal equal) name

  let string = define "string" ~empty:""
      ~inspect:sexp_of_string ~order:(fun x y ->
          match String.is_empty x, String.is_empty y with
          | true, true -> EQ
          | true,false -> GT
          | false,true -> LT
          | false,false -> partial_of_total String.compare x y)

  let bool = optional ~inspect:sexp_of_bool ~equal:Bool.equal "bool"
end

module Persistent = struct
  type 'a t =
    | String : string t
    | Define : {
        of_string : string -> 'a;
        to_string : 'a -> string;
      } -> 'a t
    | Derive : {
        of_persistent : 'b -> 'a;
        to_persistent : 'a -> 'b;
        persistent : 'b t;
      } -> 'a t


  let string = String

  let define ~to_string ~of_string = Define {
      to_string;
      of_string;
    }

  let derive ~to_persistent ~of_persistent persistent = Derive {
      to_persistent;
      of_persistent;
      persistent;
    }

  let of_binable
    : type a. (module Binable.S with type t = a) -> a t =
    fun r -> Define {
        to_string = Binable.to_string r;
        of_string = Binable.of_string r
      }

  let rec to_string
    : type a. a t -> a -> string =
    fun p x -> match p with
      | String -> x
      | Define {to_string} -> to_string x
      | Derive {to_persistent; persistent} ->
        to_string persistent (to_persistent x)

  let rec of_string
    : type a. a t -> string -> a =
    fun p s -> match p with
      | String -> s
      | Define {of_string} -> of_string s
      | Derive {of_persistent; persistent} ->
        of_persistent (of_string persistent s)



  module Chunk = struct
    (* bin_io will pack len+data, and restore it correspondingly *)
    type t = {data : string} [@@deriving bin_io]
  end

  module KV = struct
    type t = {key : string; data : string}
    [@@deriving bin_io]
  end

  module Chunks = struct
    type t = Chunk.t list [@@deriving bin_io]
  end
  module KVS = struct
    type t = KV.t list [@@deriving bin_io]
  end

  let chunks = of_binable (module Chunks)
  let kvs = of_binable (module KVS)

  let list p = derive chunks
      ~to_persistent:(List.rev_map ~f:(fun x ->
          {Chunk.data = to_string p x}))
      ~of_persistent:(List.rev_map ~f:(fun {Chunk.data} ->
          of_string p data))

  let array p = derive chunks
      ~to_persistent:(Array.fold ~init:[] ~f:(fun xs x ->
          {Chunk.data = to_string p x} :: xs))
      ~of_persistent:(Array.of_list_rev_map ~f:(fun {Chunk.data} ->
          of_string p data))

  let sequence p = derive chunks
      ~to_persistent:(fun xs ->
          Sequence.to_list_rev @@
          Sequence.map xs ~f:(fun x ->
              {Chunk.data = to_string p x}))
      ~of_persistent:(fun xs ->
          Sequence.of_list @@
          List.rev_map xs ~f:(fun {Chunk.data} ->
              of_string p data))

  let set c p = derive (list p)
      ~to_persistent:Set.to_list
      ~of_persistent:(Set.of_list c)

  let map c pk pd = derive kvs
      ~to_persistent:(Map.fold ~init:[] ~f:(fun ~key ~data xs -> {
            KV.key = to_string pk key;
            KV.data = to_string pd data
          } :: xs))
      ~of_persistent:(List.fold ~init:(Map.empty c)
                        ~f: (fun xs {KV.key;data} ->
                            let key = of_string pk key
                            and data = of_string pd data in
                            Map.add_exn xs ~key ~data))
end


type 'a obj = Oid.t

type fullname = {
  package : string;
  name : string;
} [@@deriving sexp_of]


module Registry = struct
  let packages = Hash_set.create (module String) ()
  let classes = Hashtbl.create (module String)
  let slots = Hashtbl.create (module String)

  let add_package name = Hash_set.add packages name

  let is_present ~package namespace name =
    match Hashtbl.find namespace package with
    | None -> false
    | Some names -> Map.mem names name

  let register kind namespace ?desc ?(package=user_package) name =
    add_package package;
    if is_present ~package namespace name
    then failwithf
        "Failed to declare new %s, there is already a %s \
         named `%s' in package `%s'" kind kind name package ();
    Hashtbl.update namespace package ~f:(function
        | None -> Map.singleton (module String) name desc
        | Some names -> Map.add_exn names ~key:name ~data:desc);
    {package; name}

  let add_class = register "class" classes
  let add_slot  = register "property" slots
end

let string_of_fname {package; name} =
  if package = keyword_package || package = user_package
  then name
  else package ^ ":" ^ name

let escaped =
  Staged.unstage @@
  String.Escaping.escape ~escapeworthy:[':'] ~escape_char:'\\'

let find_separator s =
  if String.is_empty s then None
  else String.Escaping.index s ~escape_char:'\\' ':'

(* invariant, keywords are always prefixed with [:] *)
let normalize_name ~package name =
  let package = escaped package in
  if package = keyword_package &&
     not (String.is_prefix ~prefix:":" name)
  then ":"^name else name

let split_name package s = match find_separator s with
  | None -> {package; name=s}
  | Some 0 -> {package=keyword_package; name=s}
  | Some len -> {
      package = String.sub s ~pos:0 ~len;
      name = String.subo s ~pos:(len+1);
    }

module Class = struct
  type +'s info = {
    id : Cid.t;
    name : fullname;
    sort : 's;
  }
  let id {id} = id

  type (+'a,+'s) t = 's info

  let classes = ref Cid.zero

  let newclass ?desc ?package name sort =
    Cid.incr classes;
    {
      id = !classes;
      name = Registry.add_class ?desc ?package name;
      sort;
    }

  let declare
    : ?desc:string -> ?package:string -> string -> 's -> ('k,'s) t =
    fun ?desc ?package name data ->
    newclass ?desc ?package name data

  let refine {id; name} sort = {id; name; sort}

  let same x y = Cid.equal x.id y.id

  let equal : type a b. (a,_) t -> (b,_) t -> (a obj,b obj) Type_equal.t option =
    fun x y -> Option.some_if (same x y) Type_equal.T

  let assert_equal x y = match equal x y with
    | Some t -> t
    | None ->
      failwithf "assert_equal: wrong assertion, classes of %s and %s \
                 are different"
        (string_of_fname x.name)
        (string_of_fname y.name)
        ()



  let sort = fun {sort} -> sort
  let name {name={name}} = name
  let package {name={package}} = package
  let fullname {name} = string_of_fname name

end

module Dict = struct
  module Key = struct
    module Uid = Int
    let last_id = ref 0

    type 'a t = {
      ord : Uid.t;
      key : 'a Type_equal.Id.t;
    }

    let create ~name sexp_of_t =
      let key = Type_equal.Id.create ~name sexp_of_t in
      incr last_id;
      {key; ord = !last_id}

    let uid {ord} = ord
    let name x = Type_equal.Id.name x.key
    let to_sexp x = Type_equal.Id.to_sexp x.key
    let same x y = Type_equal.Id.same_witness_exn x.key y.key
  end
  type 'a key = 'a Key.t
  type record =
    | T0
    | T1 : 'a key * 'a -> record
    | T2 : 'a key * 'a *
           'b key * 'b -> record
    | T3 : 'a key * 'a *
           'b key * 'b *
           'c key * 'c -> record
    | T4 : 'a key * 'a *
           'b key * 'b *
           'c key * 'c *
           'd key * 'd -> record
    | LL : record * 'a key * 'a * record -> record
    | LR : record * 'a key * 'a * record -> record

  type t = record

  let empty = T0

  (*
     - LL (x,y) : h(x) = h(y) - 1, if balanced
                | h(x) = h(y) - 2, otherwise
     - LR (x,y) : h(x) = h(y), if balanced
                | h(x) = h(y) + 1, otherwise
 *)

  let uid = Key.uid

  let compare_keys k1 k2 =
    let k1 = uid k1 and k2 = uid k2 in
    Key.Uid.compare k1 k2
  [@@inline]

  let (<$) k1 k2 =
    let k1 = uid k1 and k2 = uid k2 in
    Key.Uid.(k1 < k2)
  [@@inline]

  let make0 = T0 [@@inlined]
  let make1 k a = T1 (k,a) [@@inline]
  let make2 ka a kb b = T2 (ka,a,kb,b) [@@inline]
  let make3 ka a kb b kc c = T3 (ka,a,kb,b,kc,c) [@@inline]
  let make4 ka a kb b kc c kd d = T4 (ka,a, kb,b, kc,c, kd,d) [@@inline]
  let make5 ka a kb b kc c kd d ke e =
    LR (T0,ka,a,make4 kb b kc c kd d ke e)
  [@@inline]
  let make6 ka a kb b kc c kd d ke e kf f =
    LR (T1 (ka,a),kb,b,T4(kc,c,kd,d,ke,e,kf,f))
  [@@inline]
  let make7 ka a kb b kc c kd d ke e kf f kg g =
    LR (T2 (ka,a,kb,b),kc,c, T4 (kd,d,ke,e,kf,f,kg,g))
  [@@inline]
  let make8 ka a kb b kc c kd d ke e kf f kg g kh h =
    LR (T3 (ka,a,kb,b,kc,c),kd,d, T4(ke,e,kf,f,kg,g,kh,h))
  [@@inline]
  let make9 ka a kb b kc c kd d ke e kf f kg g kh h ki i =
    LR (T4 (ka,a,kb,b,kc,c,kd,d),ke,e,T4(kf,f,kg,g,kh,h,ki,i))
  [@@inline]
  let make10 ka a kb b kc c kd d ke e kf f kg g kh h ki i kj j =
    LL (T0,ka,a,make9 kb b kc c kd d ke e kf f kg g kh h ki i kj j)
  [@@inline]

  type 'r visitor = {
    visit : 'a. 'a key -> 'a -> 'r -> 'r;
  } [@@unboxed]

  let above x k = match x with
    | T0 -> false (* vacuous truth *)
    | T1 (x,_) -> x <$ k
    | T2 (_,_,x,_) -> x <$ k
    | T3 (_,_,_,_,x,_) -> x <$ k
    | T4 (_,_,_,_,_,_,x,_) -> x <$ k
    | LL (_,x,_,_) -> x <$ k
    | LR (_,x,_,_) -> x <$ k
  [@@inline]

  let below k = function
    | T0 -> false                (* vacuous truth *)
    | T1 (x,_) -> k <$ x
    | T2 (x,_,_,_) -> k <$ x
    | T3 (x,_,_,_,_,_) -> k <$ x
    | T4 (x,_,_,_,_,_,_,_) -> k <$ x
    | LL (_,x,_,_) -> k <$ x
    | LR (_,x,_,_) -> k <$ x
  [@@inline]

  let rec foreach x ~init f = match x with
    | T0 -> init
    | T1 (ka,a) -> f.visit ka a init
    | T2 (ka,a,kb,b) ->
      f.visit ka a init |>
      f.visit kb b
    | T3 (ka,a,kb,b,kc,c) ->
      f.visit ka a init |>
      f.visit kb b |>
      f.visit kc c
    | T4 (ka,a,kb,b,kc,c,kd,d) ->
      f.visit ka a init |>
      f.visit kb b |>
      f.visit kc c |>
      f.visit kd d
    | LL (x,k,a,y) ->
      let init = f.visit k a init in
      foreach y ~init:(foreach x ~init f) f
    | LR (x,k,a,y) ->
      let init = f.visit k a init in
      foreach y ~init:(foreach x ~init f) f

  type ('b,'r) app = {
    app : 'a. 'a key -> 'a -> 'b -> 'r
  } [@@unboxed]

  let cmp x y = compare_keys x y [@@inline]
  let eq x y = compare_keys x y = 0 [@@inline]

  let rec has_slot = function
    | T0 | T1 _ | T2 _ | T3 _ -> true
    | LR (x,_,_,y) -> has_slot x || has_slot y
    | LL (x,_,_,y) -> has_slot x || has_slot y
    | _ -> false


  exception Bal of record
  exception LL_not_balanced of record * record
  exception LR_not_balanced of record * record
  exception LL_balanced of record * record
  exception LR_balanced of record * record
  exception Not_disbalanced of record
  exception Not_balanced of record
  exception Broken_bal of record * record
  exception Broken_ins of record * record

  let rec height = function
    | T0 | T1 _ | T2 _ | T3 _ | T4 _ -> 1
    | LL (x,_,_,y) ->
      let h1 = height x
      and h2 = height y in
      if h1 = h2 - 1
      then h2 + 1
      else raise (LL_not_balanced (x,y))
    | LR (x,_,_,y) ->
      let h1 = height x
      and h2 = height y in
      if h1 = h2
      then h2 + 1
      else raise (LR_not_balanced (x,y))

  let rec h = function
    | T0 | T1 _ | T2 _ | T3 _ | T4 _ -> 1
    | LL (x,_,_,y) -> max (h x) (h y) + 1
    | LR (x,_,_,y) -> max (h x) (h y) + 1

  let is_disbalanced t = match t with
    | T0 | T1 _ | T2 _ | T3 _ | T4 _ -> false
    | LL (x,_,_,y) -> height x = height y - 2
    | LR (x,_,_,y) -> height x = height y + 1

  let is_balanced t = match t with
    | T0 | T1 _ | T2 _ | T3 _ | T4 _ -> true
    | LL (x,_,_,y) -> height x = height y - 1
    | LR (x,_,_,y) -> height x = height y

  let rec bal : record -> record = function
    (* first floor cases are handled manually *)
    (* | LR(LR(T1 (ka,a),kb,b,T3 (kc,c,kd,d,ke,e)),kf,f,T0) ->
     *   make6 ka a kb b kc c kd d ke e kf f
     * | LR(LR(T0,ka,a,T4 (kb,b,kc,c,kd,d,ke,e)),kf,f,T0) ->
     *   make6 ka a kb b kc c kd d ke e kf f
     * | LR(LR(T0,ka,a,T4 (kb,b,kc,c,kd,d,ke,e)),kf,f,T1 (kg,g)) ->
     *   make7 ka a kb b kc c kd d ke e kf f kg g
     * | LR(LR(T0,ka,a,T4 (kb,b,kc,c,kd,d,ke,e)),kf,f,T2 (kg,g,kh,h)) ->
     *   make8 ka a kb b kc c kd d ke e kf f kg g kh h
     * | LR(LR(T0,ka,a,T4 (kb,b,kc,c,kd,d,ke,e)),kf,f,T3 (kg,g,kh,h,ki,i)) ->
     *   make9 ka a kb b kc c kd d ke e kf f kg g kh h ki i
     * | LR(LR(T0,ka,a,T4 (kb,b,kc,c,kd,d,ke,e)),kf,f,T4 (kg,g,kh,h,ki,i,kj,j)) ->
     *   make10 ka a kb b kc c kd d ke e kf f kg g kh h ki i kj j *)


    | LL (x,ka,a,LL(y,kb,b,z)) ->
      (*
         h(x) = m
         h(LL(y,b,z)) = m+2
         h(y) = m
         h(z) = m+1
         -----------------
         h(LR(x,a,y)) = m+1
         LR (LR(x,a,y),b,z)
      *)
      LR (LR (x,ka,a,y),kb,b,z)

    | LL (w,ka,a,LR(LR(x,kb,b,y),kc,c,z)) ->
      (*
         h(w) = m
         h(LR(LR(x,b,y),c,z)) = m+2
         h(LR(x,b,y)) = m+1
         h(z) = m+1
         h(x) = m
         h(y) = m
         -----------------
         h(LR(w,a,x) = m+1
         h(LL(y,c,z) = m+2
         h(LR(w,a,x),b,LL(y,c,z))=m+3
      *)
      LL (LR(w,ka,a,x),kb,b,LL(y,kc,c,z))

    | LL (w,ka,a,LR(LL(x,kb,b,y),kc,c,z)) ->
      (*
         h(w) = m
         h(LR(LL(x,b,y),c,z)) = m+2
         h(z) = m+1
         h(LL(x,b,y)) = m+1
         h(y) = m
         h(x) = m-1
         -----------------------
         h(LR+(w,a,x))=m+1
         h(LL(y,c,z))=m+2
         LL (LR+(w,a,x),b,LL(y,c,z))
      *)
      LL (bal(LR(w,ka,a,x)),kb,b,LL(y,kc,c,z))

    | LL (x,ka,a,LR(y,kb,b,z)) ->
      (*
         h(x) = m
         h(LR(y,b,z)) = m+2
         h(y) = m+1
         h(z) = m+1
         -----------------
         LR+(LL(x,a,y),b,z)
*)

      bal (LR (LL (x,ka,a,y),kb,b,z))

    | LR(LL(w,ka,a,LL(x,kb,b,y)),kc,c,z) ->
      (*
         h(z) = m
         h(LL(w,a,LL(x,b,y))) = m+1
         h(LL(x,b,y)) = m
         h(w) = m-1
         h(x) = m-2
         h(y) = m-1
         --------------------------
         h(LL(y,c,z)) = m+1
         h(LR+(w,a,x)) = m
         LL(LR+(w,a,x),b,LL(y,c,z))
      *)
      LL(bal(LR(w,ka,a,x)),kb,b,LL(y,kc,c,z))

    | LR(LL(w,ka,a,LR(x,kb,b,y)),kc,c,z) ->
      (*
         h(z) = m
         h(LL(w,a,LR(x,b,y))) = m+1
         h(LR(x,b,y)) = m
         h(w) = m-1
         h(x) = m-1
         h(y) = m-1
         --------------------------
         h(LR(w,a,x)) = m
         LL(y,c,z) = m+1
         LL(LR(w,a,x),b,LL(y,c,z))
      *)
      LL(LR(w,ka,a,x),kb,b,LL(y,kc,c,z))

    | LR(LR(w,ka,a,LR(x,kb,b,y)),kc,c,z) ->
      (*
         h(z) = m
         h(LR(w,a,LR(x,b,y))) = m+1
         h(LR(x,b,y)) = m
         h(w) = m
         h(x) = m-1
         h(y) = m-1
         --------------------------
         h(LR+(w,a,x)) = m+1
         h(LL(y,c,z)) = m+1
         LR(LR+(w,a,x),b,LL(y,c,z))
      *)
      LR(bal(LR(w,ka,a,x)),kb,b,LL(y,kc,c,z))

    | LR(LR(LR(w,ka,a,x),kb,b,y),kc,c,z) ->
      (*
         h(z) = m
         h(LR(LR(w,a,x),b,y)) = m+1
         h(y) = m
         h(LR(w,a,x)) = m
         h(w) = m-1
         h(x) = m-1
         --------------------------
         h(LR(w,a,x)) = m
         h(LR(y,c,z) = m+1
         LL(LR(w,a,x),b,LR(y,c,z))
      *)
      LL(LR(w,ka,a,x),kb,b,LR(y,kc,c,z))

    | LR(LR(LL(w,ka,a,x),kb,b,y),kc,c,z) ->
      (*
         h(z) = m
         h(LR(LL(w,a,x),b,y)) = m+1
         h(y) = m
         h(LL(w,a,x)) = m
         h(w) = m-2
         h(x) = m-1
         --------------------------
         h(LL(w,a,x)) = m
         h(LR(y,c,z)) = m+1
         LL(LL(w,a,x),b,LR(y,c,z))
      *)
      LL(LL(w,ka,a,x),kb,b,LR(y,kc,c,z))

    | LR(LL(LR(w,ka,a,x),kb,b,y),kc,c,z) ->
      (*
         h(z) = m
         h(LL(LR(w,a,x),b,y)) = m+1
         h(y) = m
         h(LR(w,a,x)) = m-1
         h(w) = m-2
         h(x) = m-2
         --------------------------
         h(LR(w,a,x))=m-1
         h(LR(y,c,z))=m+1
         LL+(LR(w,a,x),b,LR(y,c,z))
      *)
      bal(LL(LR(w,ka,a,x),kb,b,LR(y,kc,c,z)))

    | LR(LL(LL(w,ka,a,x),kb,b,y),kc,c,z) ->
      (*
         h(z) = m
         h(LL(LL(w,a,x),b,y)) = m+1
         h(y) = m
         h(LL(w,a,x)) = m-1
         h(w) = m-3
         h(x) = m-2
         --------------------------
         h(LL(w,ka,a,x)) = m-1
         h(LR(y,c,z)) = m+1
         LL+(LL(w,a,x),b,LR(y,c,z))
      *)
      bal (LL(LL(w,ka,a,x),kb,b,LR(y,kc,c,z)))

    | LR(LR(x,ka,a,y),kb,b,z) ->
         (*
         h(z) = m
         h(LR(x,ka,a,y) = m+1
         h(x) = m
         h(y) = m
         --------------
         h(LR(y,b,z)) = m+1
         LL(x,a,LR(y,b,z))
    *)
      LL(x,ka,a,LR(y,kb,b,z))

    (* | LR (LR (x,ka,a,y),kb,b,z) ->
     *   assert false *)


    | t -> raise (Bal t)


  (*

     h(LR(x,ka,a,y)) = m+1
     h(z) = m
     h(y) = m
     h(x) = m
     ---------------------
     LR(y,b,z) = m+1
     LL(x,a,LR(y,b,z))
  *)

  let rec ror = function
    | LR (LR(x,ka,a,y),kb,b,z) ->
      (* h(LR(x,ka,a,y)) = m+1
       * h(z) = m
       * h(y) = m
       * h(x) = m
       * ---------------------
       * LR(y,b,z) = m+1
       * LL(x,a,LR(y,b,z)) *)
      LL(x,ka,a,LR(y,kb,b,z))
    | LR (w,kb,b,z) ->
      (* ror (LR (rol w,kb,b,z)):
       * h(w=LL(x',a,y')) = m + 1
       * h(z) = m
       * h(x') = m-1
       * h(y') = m
       * rol(w) => LR+(x,ka,a,y)
       * h(x) = h(x')+1 = m
       * h(y) = h(y')-1 = m-1
       * ror(LR+(x,a,y),b,z) => LL(x,a,LL(y,b,z))
      *)
      ror (LR (rol w,kb,b,z))
  and rol = function
    | _ -> assert false



  let bal x =
    if not (is_disbalanced x)
    then raise (Not_disbalanced x);
    let x' = bal x in
    if not (is_balanced x')
    then raise (Broken_bal (x,x'));
    let h = h x and h' = h x' in
    Format.eprintf "bal: %d => %d@\n%!" h h';

    x'


  (* pre:
     - a is not in t;
     - for all functions except [bal] t is balanced;
     - for [bal] the input is t is disbalanced.

     post:
     - a is in t', and len t' = len t + 1
     - h(t') >= h(t)
     - t' is balanced
  *)
  let rec insert
    : type a. a key -> a -> record -> record = fun ka a -> function
    | T0 -> make1 ka a
    | T1 (kb,b) -> if ka <$ kb
      then make2 ka a kb b
      else make2 kb b ka a
    | T2 (kb,b, kc,c) -> if ka <$ kb
      then make3 ka a kb b kc c else if ka <$ kc
      then make3 kb b ka a kc c
      else make3 kb b kc c ka a
    | T3 (kb,b,kc,c,kd,d) -> if ka <$ kb
      then make4 ka a kb b kc c kd d else if ka <$ kc
      then make4 kb b ka a kc c kd d else if ka <$ kd
      then make4 kb b kc c ka a kd d
      else make4 kb b kc c kd d ka a
    | T4 (kb,b,kc,c,kd,d,ke,e) -> if ka <$ kb
      then make5 ka a kb b kc c kd d ke e else if ka <$ kc
      then make5 kb b ka a kc c kd d ke e else if ka <$ kd
      then make5 kb b kc c ka a kd d ke e else if ka <$ ke
      then make5 kb b kc c kd d ka a ke e
      else make5 kb b kc c kd d ke e ka a
    | LR (T0,kb,b,T4(kc,c,kd,d,ke,e,kf,f)) ->
      if ka <$ kd then
        if ka <$ kc then
          if ka <$ kb
          then make6 ka a kb b kc c kd d ke e kf f
          else make6 kb b ka a kc c kd d ke e kf f
        else make6 kb b kc c ka a kd d ke e kf f
      else
      if ka <$ ke then
        make6 kb b kc c kd d ka a ke e kf f
      else if ka <$ kf
      then make6 kb b kc c kd d ke e ka a kf f
      else make6 kb b kc c kd d ke e kf f ka a
    | LR (T1 (kb,b),kc,c,T4(kd,d,ke,e,kf,f,kg,g)) ->
      if ka <$ kd then
        if ka <$ kc then
          if ka <$ kb
          then make7 ka a kb b kc c kd d ke e kf f kg g
          else make7 kb b ka a kc c kd d ke e kf f kg g
        else make7 kb b kc c ka a kd d ke e kf f kg g
      else
      if ka <$ kf then
        if ka <$ ke
        then make7 kb b kc c kd d ka a ke e kf f kg g
        else make7 kb b kc c kd d ke e ka a kf f kg g
      else if ka <$ kg
      then make7 kb b kc c kd d ke e kf f ka a kg g
      else make7 kb b kc c kd d ke e kf f kg g ka a
    | LR (T4 (kb,b,kc,c,kd,d,ke,e),kf,f,T1(kg,g)) ->
      if ka <$ kd then
        if ka <$ kc then
          if ka <$ kb
          then make7 ka a kb b kc c kd d ke e kf f kg g
          else make7 kb b ka a kc c kd d ke e kf f kg g
        else make7 kb b kc c ka a kd d ke e kf f kg g
      else
      if ka <$ kf then
        if ka <$ ke
        then make7 kb b kc c kd d ka a ke e kf f kg g
        else make7 kb b kc c kd d ke e ka a kf f kg g
      else if ka <$ kg
      then make7 kb b kc c kd d ke e kf f ka a kg g
      else make7 kb b kc c kd d ke e kf f kg g ka a
    | LR (T2 (kb,b,kc,c),kd,d,T4(ke,e,kf,f,kg,g,kh,h)) ->
      if ka <$ ke then
        if ka <$ kc then
          if ka <$ kb
          then make8 ka a kb b kc c kd d ke e kf f kg g kh h
          else make8 kb b ka a kc c kd d ke e kf f kg g kh h
        else
        if ka <$ kd
        then make8 kb b kc c ka a kd d ke e kf f kg g kh h
        else make8 kb b kc c kd d ka a ke e kf f kg g kh h
      else
      if ka <$ kg then
        if ka <$ kf
        then make8 kb b kc c kd d ke e ka a kf f kg g kh h
        else make8 kb b kc c kd d ke e kf f ka a kg g kh h
      else if ka <$ kh
      then make8 kb b kc c kd d ke e kf f kg g ka a kh h
      else make8 kb b kc c kd d ke e kf f kg g kh h ka a
    | LR (T4 (kb,b,kc,c,kd,d,ke,e),kf,f,T2(kg,g,kh,h)) ->
      if ka <$ ke then
        if ka <$ kc then
          if ka <$ kb
          then make8 ka a kb b kc c kd d ke e kf f kg g kh h
          else make8 kb b ka a kc c kd d ke e kf f kg g kh h
        else
        if ka <$ kd
        then make8 kb b kc c ka a kd d ke e kf f kg g kh h
        else make8 kb b kc c kd d ka a ke e kf f kg g kh h
      else
      if ka <$ kg then
        if ka <$ kf
        then make8 kb b kc c kd d ke e ka a kf f kg g kh h
        else make8 kb b kc c kd d ke e kf f ka a kg g kh h
      else if ka <$ kh
      then make8 kb b kc c kd d ke e kf f kg g ka a kh h
      else make8 kb b kc c kd d ke e kf f kg g kh h ka a
    | LR (T3 (kb,b,kc,c,kd,d),ke,e,T4(kf,f,kg,g,kh,h,ki,i)) ->
      if ka <$ ke then
        if ka <$ kc then
          if ka <$ kb
          then make9 ka a kb b kc c kd d ke e kf f kg g kh h ki i
          else make9 kb b ka a kc c kd d ke e kf f kg g kh h ki i
        else
        if ka <$ kd
        then make9 kb b kc c ka a kd d ke e kf f kg g kh h ki i
        else make9 kb b kc c kd d ka a ke e kf f kg g kh h ki i
      else
      if ka <$ kg then
        if ka <$ kf
        then make9 kb b kc c kd d ke e ka a kf f kg g kh h ki i
        else make9 kb b kc c kd d ke e kf f ka a kg g kh h ki i
      else if ka <$ kh
      then make9 kb b kc c kd d ke e kf f kg g ka a kh h ki i
      else if ka <$ ki then
        make9 kb b kc c kd d ke e kf f kg g kh h ka a ki i
      else
        make9 kb b kc c kd d ke e kf f kg g kh h ki i ka a
    | LR (T4 (kb,b,kc,c,kd,d,ke,e),kf,f,T3(kg,g,kh,h,ki,i)) ->
      if ka <$ ke then
        if ka <$ kc then
          if ka <$ kb
          then make9 ka a kb b kc c kd d ke e kf f kg g kh h ki i
          else make9 kb b ka a kc c kd d ke e kf f kg g kh h ki i
        else
        if ka <$ kd
        then make9 kb b kc c ka a kd d ke e kf f kg g kh h ki i
        else make9 kb b kc c kd d ka a ke e kf f kg g kh h ki i
      else
      if ka <$ kg then
        if ka <$ kf
        then make9 kb b kc c kd d ke e ka a kf f kg g kh h ki i
        else make9 kb b kc c kd d ke e kf f ka a kg g kh h ki i
      else if ka <$ kh
      then make9 kb b kc c kd d ke e kf f kg g ka a kh h ki i
      else if ka <$ ki then
        make9 kb b kc c kd d ke e kf f kg g kh h ka a ki i
      else
        make9 kb b kc c kd d ke e kf f kg g kh h ki i ka a
    | LR (T4 (kb,b,kc,c,kd,d,ke,e),kf,f,T4(kg,g,kh,h,ki,i,kj,j)) ->
      if ka <$ kf then
        if ka <$ kc then
          if ka <$ kb
          then make10 ka a kb b kc c kd d ke e kf f kg g kh h ki i kj j
          else make10 kb b ka a kc c kd d ke e kf f kg g kh h ki i kj j
        else
        if ka <$ ke
        then if ka <$ kd
          then make10 kb b kc c ka a kd d ke e kf f kg g kh h ki i kj j
          else make10 kb b kc c kd d ka a ke e kf f kg g kh h ki i kj j
        else make10 kb b kc c kd d ke e ka a kf f kg g kh h ki i kj j
      else
      if ka <$ ki then
        if ka <$ kh
        then
          if ka <$ kg
          then make10 kb b kc c kd d ke e kf f ka a kg g kh h ki i kj j
          else make10 kb b kc c kd d ke e kf f kg g ka a kh h ki i kj j
        else make10 kb b kc c kd d ke e kf f kg g kh h ka a ki i kj j
      else if ka <$ kj
      then make10 kb b kc c kd d ke e kf f kg g kh h ki i ka a kj j
      else make10 kb b kc c kd d ke e kf f kg g kh h ki i kj j ka a
    | LL (b,k,x,c) ->
      if ka <$ k
      then if above b ka
        then if has_slot c
          then LL (b,ka,a,insert k x c)
          else bal (LL (b,ka,a,insert k x c))
        else if has_slot b
        then LL (insert ka a b,k,x,c)
        else LR (insert ka a b,k,x,c)
      else if below ka c
      then if has_slot b
        then LL (insert k x b,ka,a,c)
        else LR (insert k x b,ka,a,c)
      else if has_slot c
      then LL (b,k,x,insert ka a c)
      else bal (LL (b,k,x,insert ka a c))
    | LR (b,k,x,c) ->
      if ka <$ k
      then if above b ka
        then if has_slot c
          then LR (b,ka,a,insert k x c)
          else LL (b,ka,a,insert k x c)
        else if has_slot b
        then  LR (insert ka a b,k,x,c)
        else  bal (LR (insert ka a b,k,x,c))
      else if below ka c
      then if has_slot b
        then LR (insert k x b,ka,a,c)
        else bal (LR (insert k x b,ka,a,c))
      else if has_slot c
      then LR (b,k,x,insert ka a c)
      else LL (b,k,x,insert ka a c)

  let insert k x t =
    if not (is_balanced t)
    then raise (Not_balanced t);
    let t' = insert k x t in
    if not (is_balanced t')
    then raise (Broken_ins (t,t'));
    t'

  type test = {
    pos : int;
    key : int key;
    add : record -> record;
    (* put : int -> record -> record; *)
    (* get : record -> int; *)
    core_add : Univ_map.t -> Univ_map.t;
    (* core_get : Univ_map.t -> int; *)
  }

  let input n =
    Sequence.range 0 n |>
    Sequence.map ~f:(fun pos ->
        let name = sprintf "f%d" pos in
        let k = Key.create ~name sexp_of_int in
        let core_add r = Univ_map.add_exn r k.key pos in
        (* let core_get r = Univ_map.find_exn r k.key in *)
        {
          pos;
          key = k;
          add = insert k pos;
          (* add = update (fun _ x -> x) k pos; *)
          (* put = update (fun _ x -> x) k; *)
          (* get = get k; *)
          core_add;
          (* core_get; *)
        }) |>
    Sequence.to_array

  let rec length = function
    | T0 -> 0
    | T1 _ -> 1
    | T2 _ -> 2
    | T3 _ -> 3
    | T4 _ -> 4
    | LR (x,_,_,y)
    | LL (x,_,_,y) ->  length x + length y + 1

  let test input =
    let input = Array.copy input in
    Array.permute input;
    trace_enter "test/dict";
    let x = Array.fold input ~init:T0 ~f:(fun r {add} -> add r) in
    trace_leave "test/dict";
    trace_enter "test/core";
    let y = Array.fold input ~init:Univ_map.empty ~f:(fun r {core_add} ->
        core_add r) in
    trace_leave "test/core";
    input,x,y

  let r =
    trace_reset ();
    Format.eprintf "@\n%!";
    let _,d,m = test (input 10000) in
    let size x = Obj.reachable_words (Obj.repr x) in
    Format.eprintf "dict=%d vs. core=%d@\n%!" (size d) (size m);
    if (length d <> List.length (Univ_map.to_alist m))
    then failwith "the map is incomplete";;

  (* [merge k x y] *)
  type merge = {
    merge : 'a. 'a key -> 'a -> 'a -> 'a
  } [@@unboxed]
  (* we could use a GADT, but it couldn't be unboxed,
     an since we could create merge functions a lot,
     it is better not to allocate them.*)

  let merge
    : type a b. merge -> a key -> b key -> b -> a -> a =
    fun {merge} ka kb b a ->
      let T = Key.same ka kb in
      merge kb b a

  let app = merge

  let rec upsert ~update:ret ~insert:add ka a t =
    match t with
    | T0 -> add (make1 ka a)
    | T1 (kb,b) -> if eq ka kb
      then ret (fun f -> make1 ka (app f ka kb b a))
      else add (insert ka a t)
    | T2 (kb,b,kc,c) -> if eq ka kb
      then ret (fun f -> make2 ka (app f ka kb b a) kc c) else if eq ka kc
      then ret (fun f -> make2 kb b ka (app f ka kc c a))
      else add (insert ka a t)
    | T3 (kb,b,kc,c,kd,d) -> begin match cmp ka kc with
        | 0 -> ret (fun f -> make3 kb b ka (app f ka kc c a) kd d)
        | 1 -> if eq ka kd
          then ret (fun f -> make3 kb b kc c ka (app f ka kd d a))
          else add (insert ka a t)
        | _ -> if eq ka kb
          then ret (fun f -> make3 ka (app f ka kb b a) kc c kd d)
          else add@@insert ka a t
      end
    | T4 (kb,b,kc,c,kd,d,ke,e) -> begin match cmp ka kd with
        | 0 -> ret@@fun f ->
          make4 kb b kc c ka (app f ka kd d a) ke e
        | 1 -> if eq ka ke
          then ret@@fun f -> make4 kb b kc c kd d ka (app f ka ke e a)
          else add@@insert ka a t
        | _ -> match cmp ka kc with
          | 0 -> ret@@fun f ->
            make4 kb b ka (app f ka kc c a) kd d ke e
          | 1 -> add@@insert ka a t
          | _ -> if eq ka kb
            then ret@@fun f ->
              make4 ka (app f ka kb b a) kc c kd d ke e
            else add@@insert ka a t
      end
    | LL (x,kb,b,y) -> begin match cmp ka kb with
        | 0 -> ret@@fun f -> LL (x,ka,app f ka kb b a,y)
        | 1 -> upsert ka a y
                 ~update:(fun k -> ret@@fun f -> LL (x,kb,b,k f))
                 ~insert:(fun y -> add@@bal@@LL (x,kb,b,y))
        | _ ->
          upsert ka a x
            ~update:(fun k -> ret@@fun f -> LL (k f,kb,b, y))
            ~insert:(fun x -> add@@LR (x,kb,b,y))
      end
    | LR (x,kb,b,y) -> begin match cmp ka kb with
        | 0 -> ret@@fun f -> LR (x,ka,app f ka kb b a,y)
        | 1 -> upsert ka a y
                 ~update:(fun k -> ret@@fun f -> LR (x,kb,b,k f))
                 ~insert:(fun y -> add@@LL (x,kb,b,y))
        | _ -> upsert ka a x
                 ~update:(fun k -> ret@@fun f -> LR (k f,kb,b,y))
                 ~insert:(fun x -> add@@bal@@LR (x,kb,b,y))
      end

  let monomorphic_merge
    : type t. t key -> (t -> t -> t) -> merge =
    fun k f -> {
        merge = fun (type a)
          (kb : a key) (b : a) (a : a) : a ->
          let T = Key.same k kb in
          f b a
      }

  let update f ka a x =
    let f = monomorphic_merge ka f in
    upsert ka a x
      ~update:(fun k -> k f)
      ~insert:(fun x -> x)

  let set ka a x =
    let f = monomorphic_merge ka (fun _ x -> x) in
    upsert ka a x
      ~update:(fun k -> k f)
      ~insert:(fun x -> x)

  exception Field_not_found

  let return (type a b) (k : a key) (ka : b key) (a : b) : a =
    let T = Key.same k ka in
    a
  [@@inline]

  let rec get k = function
    | T0 -> raise Field_not_found
    | T1 (ka,a) -> if eq k ka then return k ka a
      else raise Field_not_found
    | T2 (ka,a,kb,b) -> begin match cmp k kb with
        | 0 -> return k kb b
        | 1 -> raise Field_not_found
        | _ -> if eq k ka then return k ka a
          else raise Field_not_found
      end
    | T3 (ka,a,kb,b,kc,c) -> begin match cmp k kb with
        | 0 -> return k kb b
        | 1 -> if eq k kc then return k kc c
          else raise Field_not_found
        | _ -> if eq k ka then return k ka a
          else raise Field_not_found
      end
    | T4 (ka,a,kb,b,kc,c,kd,d) -> begin match cmp k kc with
        | 0 -> return k kc c
        | 1 -> if eq k kd then return k kd d
          else raise Field_not_found
        | _ -> match cmp k kb with
          | 0 -> return k kb b
          | 1 -> raise Field_not_found
          | _ -> if eq k ka then return k ka a
            else raise Field_not_found
      end
    | LL (x,ka,a,y) -> begin match cmp k ka with
        | 0 -> return k ka a
        | 1 -> get k y
        | _ -> get k x
      end
    | LR (x,ka,a,y) -> begin match cmp k ka with
        | 0 -> return k ka a
        | 1 -> get k y
        | _ -> get k x
      end


  let find k x = try Some (get k x) with
    | Field_not_found -> None

  let merge (type a) m x y =
    foreach y ~init:x {
      visit = fun (type b c) (ka : b key) (a : b) x ->
        upsert ka a x
          ~insert:(fun x -> x)
          ~update:(fun k -> k m)
    }



  let pp_field ppf (k,v) =
    Format.fprintf ppf "%s : %a"
      (Key.name k)
      Sexp.pp_hum (Key.to_sexp k v)

  let rec pp_fields ppf = function
    | T0 -> ()
    | T1 (ka,a) ->
      Format.fprintf ppf "%a" pp_field (ka,a)
    | T2 (ka,a,kb,b) ->
      Format.fprintf ppf "%a;@ %a"
        pp_field (ka,a)
        pp_field (kb,b)
    | T3 (ka,a,kb,b,kc,c) ->
      Format.fprintf ppf "%a;@ %a;@ %a"
        pp_field (ka,a)
        pp_field (kb,b)
        pp_field (kc,c)
    | T4 (ka,a,kb,b,kc,c,kd,d) ->
      Format.fprintf ppf "%a;@ %a;@ %a;@ %a"
        pp_field (ka,a)
        pp_field (kb,b)
        pp_field (kc,c)
        pp_field (kd,d)
    | LR (x,ka,a,y) ->
      Format.fprintf ppf "%a;@ %a;@ %a"
        pp_fields x pp_field (ka,a) pp_fields y
    | LL (x,ka,a,y) ->
      Format.fprintf ppf "%a;@ %a;@ %a"
        pp_fields x pp_field (ka,a) pp_fields y

  let pp ppf t =
    Format.fprintf ppf "{@[<2>@,%a@]}" pp_fields t

  let pp_elt ppf (k,v) =
    Format.fprintf ppf "%a" Sexp.pp_hum (Key.to_sexp k v)


  let rec pp_tree ppf = function
    | T0 -> Format.fprintf ppf "()"
    | T1 (ka,a) ->
      Format.fprintf ppf "(%a)" pp_elt (ka,a)
    | T2 (ka,a,kb,b) ->
      Format.fprintf ppf "(%a,%a)"
        pp_elt (ka,a)
        pp_elt (kb,b)
    | T3 (ka,a,kb,b,kc,c) ->
      Format.fprintf ppf "(%a,%a,%a)"
        pp_elt (ka,a)
        pp_elt (kb,b)
        pp_elt (kc,c)
    | T4 (ka,a,kb,b,kc,c,kd,d) ->
      Format.fprintf ppf "(%a,%a,%a,%a)"
        pp_elt (ka,a)
        pp_elt (kb,b)
        pp_elt (kc,c)
        pp_elt (kd,d)
    | LR (x,k,a,y) ->
      Format.fprintf ppf "LR(%a,%a,%a)"
        pp_tree x pp_elt (k,a) pp_tree y
    | LL (x,k,a,y) ->
      Format.fprintf ppf "LL(%a,%a,%a)"
        pp_tree x pp_elt (k,a) pp_tree y

  let pp_key ppf {Key.key} =
    Format.fprintf ppf "%s" (Type_equal.Id.name key)


  let pp_uid ppf uid =
    Format.fprintf ppf "%a" Sexp.pp_hum (Uid.sexp_of_t uid)



  ;;

end
;;

module Record = struct
  module Key = Dict.Key
  module Uid = Dict.Key.Uid

  type record = Dict.t
  type t = record
  type 'a key = 'a Dict.key

  module Repr = struct
    type entry = {
      name : string;
      data : string;
    } [@@deriving bin_io]

    type t = entry list [@@deriving bin_io]
  end

  type vtable = {
    order   : 'a. 'a key -> 'a -> 'a -> Order.partial;
    join    : 'a. 'a key -> 'a -> 'a -> ('a,conflict) result;
    inspect : 'a. 'a key -> 'a -> Sexp.t;
  }

  type slot_io = {
    reader : string -> record -> record;
    writer : record -> string option;
  }

  let io : slot_io Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let vtables : vtable Hashtbl.M(Uid).t =
    Hashtbl.create (module Uid)

  let empty = Dict.empty

  let uid = Key.uid
  let domain k = Hashtbl.find_exn vtables (uid k)

  let (<:=) x y = Dict.foreach ~init:true x {
      visit = fun k x yes ->
        yes && match Dict.find k y with
        | None -> false
        | Some y -> match (domain k).order k x y with
          | LT | EQ -> true
          | GT | NC -> false
    }

  let order : t -> t -> Order.partial = fun x y ->
    match x <:= y, y <:= x with
    | true,false  -> LT
    | true,true   -> EQ
    | false,true  -> GT
    | false,false -> NC


  let commit (type p) {Domain.join} (key : p Key.t) v x =
    match Dict.find key v with
    | None -> Ok (Dict.insert key x v)
    | Some y -> match join y x with
      | Ok x -> Ok (Dict.set key x v)
      | Error err -> Error err

  let put k v x = Dict.set k x v
  let get
    : type a. a Key.t -> a Domain.t -> record -> a =
    fun k {Domain.empty} data ->
    match Dict.find k data with
    | None -> empty
    | Some x -> x

  exception Merge_conflict of conflict

  let merge_or_keep old our =
    Dict.foreach our ~init:old {
      visit = fun kb b out -> match Dict.find kb old with
        | None -> Dict.insert kb b out
        | Some a -> match (domain kb).join kb a b with
          | Ok b -> Dict.set kb b out
          | Error _ -> out
    }

  let try_merge ~on_conflict old our =
    Dict.foreach our ~init:(Ok old) {
      visit = fun kb b out ->
        match out with
        | Error _ as err -> err
        | Ok out -> match Dict.find kb old with
          | None -> Ok (Dict.insert kb b out)
          | Some a -> match (domain kb).join kb a b with
            | Ok b -> Ok (Dict.set kb b out)
            | Error err -> match on_conflict with
              | `drop_both -> assert false
              | `drop_left -> Ok (Dict.set kb b out)
              | `drop_right -> Ok (Dict.set kb a out)
              | `fail -> Error err
    }


  let join x y = try_merge ~on_conflict:`fail x y

  let eq = Type_equal.Id.same_witness_exn

  let register_persistent (type p)
      (key : p Key.t)
      (p : p Persistent.t) =
    let slot = Key.name key in
    Hashtbl.add_exn io ~key:slot ~data:{
      reader = begin fun x dict ->
        let x = Persistent.of_string p x in
        Dict.insert key x dict
      end;
      writer = begin fun dict ->
        match Dict.find key dict with
        | None -> None
        | Some s -> Some (Persistent.to_string p s)
      end
    }

  include Binable.Of_binable(Repr)(struct
      type t = record
      let to_binable s =
        Dict.foreach s ~init:[] {
          visit = fun k _ xs ->
            let name = Key.name k in
            match Hashtbl.find io name with
            | None -> xs
            | Some {writer} ->
              match writer s with
              | None -> xs
              | Some data -> Repr.{name; data} :: xs
        }

      let of_binable entries =
        List.fold entries ~init:empty ~f:(fun s {Repr.name; data} ->
            match Hashtbl.find io name with
            | None -> s
            | Some {reader} -> reader data s)
    end)

  let eq = Dict.Key.same

  let register_domain
    : type p. p Key.t -> p Domain.t -> unit =
    fun key dom ->
    let vtable = {
      order = begin fun (type a) (k : a key) (x : a) (y : a) ->
        let T = eq k key in
        dom.order x y
      end;
      inspect = begin fun (type a) (k : a key) (x : a) ->
        let T = eq k key in
        dom.inspect x;
      end;
      join = begin fun (type a) (k : a key) (x : a) (y : a) :
        (a,conflict) result ->
        let T = eq k key in
        dom.join x y
      end;
    } in
    Hashtbl.add_exn vtables ~key:(uid key) ~data:vtable

  let sexp_of_t x =
    let s = Format.asprintf "%a" Dict.pp x in
    Sexp.Atom s

  let t_of_sexp = opaque_of_sexp

  let inspect = sexp_of_t

  let pp ppf x = Sexp.pp_hum ppf (inspect x)
  let pp_slots slots ppf x =
    let slots = Set.of_list (module String) slots in
    match (inspect x : Sexp.t) with
    | Atom _ -> assert false
    | List xs ->
      List.iter xs ~f:(function
          | Sexp.List (Atom slot :: _ ) as data when Set.mem slots slot ->
            Sexp.pp_hum ppf data
          | _ -> ())
end

module Knowledge = struct

  type +'a value = {
    cls  : 'a;
    data : Record.t;
    time : Int63.t;
  }
  type (+'a,+'s) cls = ('a,'s) Class.t
  type 'a obj = Oid.t
  type 'p domain = 'p Domain.t
  type 'a persistent = 'a Persistent.t
  type 'a ord = Oid.comparator_witness
  type conflict = Conflict.t = ..
  type pid = Pid.t
  type oid = Oid.t [@@deriving compare, sexp]

  type cell = {
    car : oid;
    cdr : oid;
  } [@@deriving compare, sexp]

  module Cell = struct
    type t = cell
    include Base.Comparable.Make(struct
        type t = cell [@@deriving compare, sexp]
      end)
  end


  module Env = struct
    type workers = {
      waiting : Set.M(Pid).t;
      current : Set.M(Pid).t;
    }
    type work = Done | Work of workers

    type objects = {
      vals : Record.t Map.M(Oid).t;
      comp : work Map.M(Dict.Key.Uid).t Map.M(Oid).t;
      syms : fullname Map.M(Oid).t;
      heap : cell Map.M(Oid).t;
      data : Oid.t Map.M(Cell).t;
      objs : Oid.t Map.M(String).t Map.M(String).t;
      pubs : Set.M(Oid).t Map.M(String).t;
    }

    let empty_class = {
      vals = Map.empty (module Oid);
      comp = Map.empty (module Oid);
      objs = Map.empty (module String);
      syms = Map.empty (module Oid);
      pubs = Map.empty (module String);
      heap = Map.empty (module Oid);
      data = Map.empty (module Cell);
    }

    type t = {
      classes : objects Map.M(Cid).t;
      package : string;
    }
  end

  type state = Env.t
  let empty : Env.t = {
    package = user_package;
    classes = Map.empty (module Cid);
  }

  module State = struct
    include Monad.State.T1(Env)(Monad.Ident)
    include Monad.State.Make(Env)(Monad.Ident)
  end

  module Knowledge = struct
    type 'a t = ('a,conflict) Result.t State.t
    include Monad.Result.Make(Conflict)(State)
  end

  type 'a knowledge = 'a Knowledge.t

  open Knowledge.Syntax

  module Slot = struct
    type 'p promise = {
      run : Oid.t -> unit Knowledge.t;
      pid : pid;
    }

    type (+'a,'p) t = {
      cls : ('a,unit) cls;
      dom : 'p Domain.t;
      key : 'p Dict.Key.t;
      name : string;
      desc : string option;
      promises : (pid, 'p promise) Hashtbl.t;
    }

    type pack = Pack : ('a,'p) t -> pack
    let repository = Hashtbl.create (module Cid)

    let register slot =
      Hashtbl.update repository slot.cls.id ~f:(function
          | None -> [Pack slot]
          | Some xs -> Pack slot :: xs)

    let enum {Class.id} = Hashtbl.find_multi repository id

    let declare ?desc ?persistent ?package cls name (dom : 'a Domain.t) =
      let slot = Registry.add_slot ?desc ?package name in
      let name = string_of_fname slot in
      let key = Dict.Key.create ~name dom.inspect in
      Option.iter persistent (Record.register_persistent key);
      Record.register_domain key dom;
      let promises = Hashtbl.create (module Pid) in
      let cls = Class.refine cls () in
      let slot = {cls; dom; key; name; desc; promises} in
      register slot;
      slot

    let cls x = x.cls
    let domain x = x.dom
    let name x = x.name
    let desc x = match x.desc with
      | None -> "no description"
      | Some s -> s
  end

  type (+'a,'p) slot = ('a,'p) Slot.t

  module Value = struct
    type +'a t = 'a value

    (* we could use an extension variant or create a new OCaml object
       instead of incrementing a second, but they are less reliable
       and heavier *)
    let next_second =
      let current = ref Int63.zero in
      fun () -> Int63.incr current; !current

    let empty cls =
      {cls; data=Record.empty; time = next_second ()}

    let order {data=x} {data=y} = Record.order x y

    let refine {data; cls; time} s=
      {data; time; cls = Class.refine cls s}

    let cls {cls} = cls
    let create cls data = {cls; data; time = next_second ()}
    let put {Slot.key} v x = {
      v with data = Record.put key v.data x;
             time = next_second ()
    }
    let get {Slot.key; dom} {data} = Record.get key dom data
    let strip
      : type a b. (a value, b value) Type_equal.t -> (a,b) Type_equal.t =
      fun T -> T

    type strategy = [`drop_left | `drop_right | `drop_both]

    let merge ?(on_conflict=`drop_old) x y =
      let on_conflict : strategy = match on_conflict with
        | `drop_old -> if Int63.(x.time < y.time)
          then `drop_left else `drop_right
        | `drop_new -> if Int63.(x.time < y.time)
          then `drop_right else `drop_left
        | #strategy as other -> other in
      match Record.try_merge ~on_conflict x.data y.data with
      | Ok data -> {
          x with time = next_second ();
                 data;
        }
      | Error _ ->
        (* try_merge fails only if `fail is passed *)
        assert false

    let merge ?on_conflict x y =
      {x with data = Record.merge_or_keep x.data y.data}

    let merge ?on_conflict x y =
      trace_enter "value/merge";
      let x = merge ?on_conflict x y in
      trace_leave "value/merge";
      x

    let join x y = match Record.join x.data y.data with
      | Ok data -> Ok {x with data; time = next_second ()}
      | Error c -> Error c

    let join x y =
      trace_enter "value/join";
      let x = join x y in
      trace_leave "value/join";
      x

    module type S = sig
      type t [@@deriving sexp]
      val empty : t
      val domain : t domain
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    module Comparator = Base.Comparator.Make1(struct
        type 'a t = 'a value
        let sexp_of_t = sexp_of_opaque
        let compare x y = match Record.order x.data y.data with
          | LT -> -1
          | EQ -> 0
          | GT -> 1
          | NC -> Int63.compare x.time y.time
      end)

    include Comparator

    type 'a ord = comparator_witness

    let derive
      : type a b. (a,b) cls ->
        (module S with type t = (a,b) cls t
                   and type comparator_witness = (a,b) cls ord) =
      fun cls ->
      let module R = struct
        type t = (a,b) cls value
        let sexp_of_t x = Record.sexp_of_t x.data
        let t_of_sexp = opaque_of_sexp
        let empty = empty cls

        include Binable.Of_binable(Record)(struct
            type t = (a,b) cls value
            let to_binable : 'a value -> Record.t =
              fun {data} -> data
            let of_binable : Record.t -> 'a value =
              fun data -> {cls; data; time = next_second ()}
          end)
        type comparator_witness = Comparator.comparator_witness
        include Base.Comparable.Make_using_comparator(struct
            type t = (a,b) cls value
            let sexp_of_t = sexp_of_t
            include Comparator
          end)
        let domain = Domain.define ~empty ~order ~join
            ~inspect:sexp_of_t
            (Class.name cls)
      end in
      (module R)

    let pp ppf x = Record.pp ppf x.data
    let pp_slots slots ppf x = Record.pp_slots slots ppf x.data
  end

  module Class = struct
    include Class
    let property = Slot.declare
    module Abstract = struct
      let property = Slot.declare
    end
  end

  let get () = Knowledge.lift (State.get ())
  let put s = Knowledge.lift (State.put s)
  let gets f = Knowledge.lift (State.gets f)
  let update f = Knowledge.lift (State.update f)

  exception Non_monotonic_update of string * Conflict.t [@@deriving sexp]

  let provide : type a p. (a,p) slot -> a obj -> p -> unit Knowledge.t =
    fun slot obj x ->
      get () >>= function {classes} as s ->
        let {Env.vals} as objs =
          match Map.find classes slot.cls.id with
          | None -> Env.empty_class
          | Some objs -> objs in
        try put {
            s with classes = Map.set classes ~key:slot.cls.id ~data:{
            objs with vals = Map.update vals obj ~f:(function
            | None -> Record.(put slot.key empty x)
            | Some v -> match Record.commit slot.dom slot.key v x with
              | Ok r -> r
              | Error err -> raise (Record.Merge_conflict err))}}
        with Record.Merge_conflict err ->
          Knowledge.fail (Non_monotonic_update (Slot.name slot, err))

  let pids = ref Pid.zero

  let register_promise (s : _ slot) run =
    Pid.incr pids;
    let pid = !pids in
    Hashtbl.add_exn s.promises pid {run; pid}

  let promise s get =
    register_promise s @@ fun obj ->
    get obj >>= provide s obj

  let objects {Class.id} =
    get () >>| fun {classes} ->
    match Map.find classes id with
    | None -> Env.empty_class
    | Some objs -> objs

  let uid {Slot.key} = Dict.Key.uid key

  let status
    : ('a,_) slot -> 'a obj -> slot_status knowledge =
    fun slot obj ->
    objects slot.cls >>| fun {comp} ->
    match Map.find comp obj with
    | None -> Sleep
    | Some slots -> match Map.find slots (uid slot) with
      | None -> Sleep
      | Some Work _ -> Awoke
      | Some Done -> Ready

  let update_slot
    : ('a,_) slot -> 'a obj -> _ -> unit knowledge =
    fun slot obj f ->
    objects slot.cls >>= fun ({comp} as objs) ->
    let comp = Map.update comp obj ~f:(fun slots ->
        let slots = match slots with
          | None -> Map.empty (module Dict.Key.Uid)
          | Some slots -> slots in
        Map.update slots (uid slot) ~f) in
    get () >>= fun s ->
    let classes = Map.set s.classes slot.cls.id {objs with comp} in
    put {s with classes}

  let enter_slot : ('a,_) slot -> 'a obj -> unit knowledge = fun s x ->
    update_slot s x @@ function
    | Some _ -> assert false
    | None ->  Work {
        waiting = Set.empty (module Pid);
        current = Set.empty (module Pid)
      }

  let leave_slot : ('a,'p) slot -> 'a obj -> unit Knowledge.t = fun s x ->
    update_slot s x @@ function
    | Some (Work _) -> Done
    | _ -> assert false

  let update_work s x f =
    update_slot s x @@ function
    | Some (Work w) -> f w
    | _ -> assert false

  let enter_promise s x p =
    update_work s x @@ fun {waiting; current} ->
    Work {waiting; current = Set.add current p}

  let leave_promise s x p =
    update_work s x @@ fun {waiting; current} ->
    Work {waiting; current = Set.remove current p}

  let enqueue_promises s x =
    update_work s x @@ fun {waiting; current} ->
    Work {waiting = Set.union current waiting; current}

  let collect_waiting
    : ('a,'p) slot -> 'a obj -> _ Knowledge.t = fun s x ->
    objects s.cls >>| fun {comp} ->
    Map.find_exn (Map.find_exn comp x) (uid s)  |> function
    | Env.Done -> assert false
    | Env.Work {waiting} ->
      Set.fold waiting ~init:[] ~f:(fun ps p ->
          Hashtbl.find_exn s.Slot.promises p :: ps)

  let dequeue_waiting s x = update_work s x @@ fun _ ->
    Work {
      waiting = Set.empty (module Pid);
      current = Set.empty (module Pid)
    }

  let initial_promises {Slot.promises} = Hashtbl.data promises

  let current : type a p. (a,p) slot -> a obj -> p Knowledge.t =
    fun slot id ->
    objects slot.cls >>| fun {Env.vals} ->
    match Map.find vals id with
    | None -> slot.dom.empty
    | Some v -> Record.get slot.key slot.dom v

  let rec collect_inner
    : ('a,'p) slot -> 'a obj -> _ -> _ =
    fun slot obj promises ->
    current slot obj >>= fun was ->
    Knowledge.List.iter promises ~f:(fun {Slot.run; pid} ->
        enter_promise slot obj pid >>= fun () ->
        run obj >>= fun () ->
        leave_promise slot obj pid) >>= fun () ->
    collect_waiting slot obj >>= fun waiting ->
    dequeue_waiting slot obj >>= fun () ->
    match waiting with
    | [] -> Knowledge.return ()
    | promises ->
      current slot obj >>= fun now ->
      match slot.dom.order now was with
      | EQ | LT -> Knowledge.return ()
      | GT | NC -> collect_inner slot obj promises


  let collect : type a p. (a,p) slot -> a obj -> p Knowledge.t =
    fun slot id ->
    status slot id >>= function
    | Ready ->
      current slot id
    | Awoke ->
      enqueue_promises slot id >>= fun () ->
      current slot id
    | Sleep ->
      enter_slot slot id >>= fun () ->
      collect_inner slot id (initial_promises slot) >>= fun () ->
      leave_slot slot id >>= fun () ->
      current slot id

  let resolve slot obj =
    collect slot obj >>| Opinions.choice

  let suggest agent slot obj x =
    current slot obj >>= fun opinions ->
    provide slot obj (Opinions.add agent x opinions)

  let propose agent s get =
    register_promise s @@ fun obj ->
    get obj >>= suggest agent s obj

  module Object = struct
    type +'a t = 'a obj
    type 'a ord = Oid.comparator_witness

    let with_new_object objs f = match Map.max_elt objs.Env.vals with
      | None -> f Oid.first_atom {
          objs
          with vals = Map.singleton (module Oid) Oid.first_atom Record.empty
        }
      | Some (key,_) ->
        let key = Oid.next key in
        f key {
          objs
          with vals = Map.add_exn objs.vals ~key ~data:Record.empty
        }

    let create : ('a,_) cls -> 'a obj Knowledge.t = fun cls ->
      objects cls >>= fun objs ->
      with_new_object objs @@ fun obj objs ->
      update @@begin function {classes} as s -> {
          s with classes = Map.set classes ~key:cls.id ~data:objs
        }
      end >>| fun () ->
      obj

    (* an interesting question, what we shall do if
       1) an symbol is deleted
       2) a data object is deleted?

       So far we ignore both deletes.
    *)
    let delete {Class.id} obj =
      update @@ function {classes} as s -> {
          s with
          classes = Map.change classes id ~f:(function
              | None -> None
              | Some objs -> Some {
                  objs with
                  vals = Map.remove objs.vals obj;
                  comp = Map.remove objs.comp obj;
                })
        }

    let scoped cls scope =
      create cls >>= fun obj ->
      scope  obj >>= fun r ->
      delete cls obj >>| fun () ->
      r

    let do_intern =
      let is_public ~package name {Env.pubs} =
        match Map.find pubs package with
        | None -> false
        | Some pubs -> Set.mem pubs name in
      let unchanged id = Knowledge.return id in
      let publicize ~package obj: Env.objects -> Env.objects =
        fun objects -> {
            objects with pubs = Map.update objects.pubs package ~f:(function
            | None -> Set.singleton (module Oid) obj
            | Some pubs -> Set.add pubs obj)
          } in
      let createsym ~public ~package name classes clsid objects s =
        with_new_object objects @@ fun obj objects ->
        let syms = Map.set objects.syms obj {package; name} in
        let objs = Map.update objects.objs package ~f:(function
            | None -> Map.singleton (module String) name obj
            | Some names -> Map.set names name obj) in
        let objects = {objects with objs; syms} in
        let objects = if public
          then publicize ~package obj objects else objects in
        put {s with classes = Map.set classes clsid objects} >>| fun () ->
        obj in

      fun ?(public=false) ?desc:_ ?package name {Class.id} ->
        get () >>= fun ({classes} as s) ->
        let package = Option.value package ~default:s.package in
        let name = normalize_name ~package name in
        let objects = match Map.find classes id with
          | None -> Env.empty_class
          | Some objs -> objs in
        match Map.find objects.objs package with
        | None -> createsym ~public ~package name classes id objects s
        | Some names -> match Map.find names name with
          | None -> createsym ~public ~package name classes id objects s
          | Some obj when not public -> unchanged obj
          | Some obj ->
            if is_public ~package obj objects then unchanged obj
            else
              let objects = publicize ~package obj objects in
              put {s with classes = Map.set classes id objects} >>| fun () ->
              obj

    (* any [:] in names here are never treated as separators,
       contrary to [read], where they are, and [do_intern] where
       a leading [:] in a name will be left for keywords *)
    let intern ?public ?desc ?package name cls =
      let name = escaped name in
      do_intern ?public ?desc ?package name cls

    let uninterned_repr cls obj =
      Format.asprintf "#<%s %a>" cls Oid.pp obj

    let repr {Class.name=cls as fname; id=cid} obj =
      get () >>= fun {package; classes} ->
      let cls = if package = cls.package then cls.name
        else string_of_fname fname in
      Knowledge.return @@
      match Map.find classes cid with
      | None -> uninterned_repr cls obj
      | Some {syms} -> match Map.find syms obj with
        | Some fname -> if fname.package = package
          then fname.name
          else string_of_fname fname
        | None -> uninterned_repr cls obj


    let read cls input =
      try
        Scanf.sscanf input "#<%s %s@>" @@ fun _ obj ->
        Knowledge.return (Oid.atom_of_string obj)
      with _ ->
        get () >>= fun {Env.package} ->
        let {package; name} = split_name package input in
        do_intern ~package name cls

    let cast : type a b. (a obj, b obj) Type_equal.t -> a obj -> b obj =
      fun Type_equal.T x -> x

    let id x = Oid.untagged x

    module type S = sig
      type t [@@deriving sexp]
      include Base.Comparable.S with type t := t
      include Binable.S with type t := t
    end

    let derive : type a. (a,_) cls ->
      (module S
        with type t = a obj
         and type comparator_witness = a ord) = fun _ ->
      let module Comparator = struct
        type t = a obj
        let sexp_of_t = Oid.sexp_of_t
        let t_of_sexp = Oid.t_of_sexp
        type comparator_witness = a ord
        let comparator = Oid.comparator
      end in
      let module R = struct
        include Comparator
        include Binable.Of_binable(Oid)(struct
            type t = a obj
            let to_binable = ident
            let of_binable = ident
          end)
        include Base.Comparable.Make_using_comparator(Comparator)
      end in
      (module R)
  end

  module Domain = struct
    include Domain

    let inspect_obj name x =
      Sexp.Atom (Format.asprintf "#<%s %a>" name Oid.pp x)

    let obj {Class.name} =
      let name = string_of_fname name in
      total ~inspect:(inspect_obj name) ~empty:Oid.zero
        ~order:Oid.compare name
  end
  module Order = Order
  module Persistent = Persistent

  module Symbol = struct
    let intern = Object.intern
    let keyword = keyword_package

    let in_package package f =
      get () >>= function {Env.package=old_package} as s ->
        put {s with package} >>= fun () ->
        f () >>= fun r ->
        update (fun s -> {s with package = old_package}) >>| fun () ->
        r


    exception Import of fullname * fullname [@@deriving sexp_of]

    let intern_symbol ~package name obj cls =
      Knowledge.return Env.{
          cls
          with objs = Map.update cls.objs package ~f:(function
              | None -> Map.singleton (module String) name obj
              | Some names -> Map.set names name obj)}



    (* imports names inside a class.

       All names that [needs_import] will be imported
       into the [package]. If the [package] already had
       the same name but with different value, then a
       [strict] import will raise an error, otherwise it
       will be overwritten with the new value.
    *)
    let import_class ~strict ~package ~needs_import
      : Env.objects -> Env.objects knowledge
      = fun cls ->
        Map.to_sequence cls.syms |>
        Knowledge.Seq.fold ~init:cls ~f:(fun cls (obj,sym) ->
            if not (needs_import cls sym obj)
            then Knowledge.return cls
            else
              let obj' = match Map.find cls.objs package with
                | None -> Oid.zero
                | Some names -> match Map.find names sym.name with
                  | None -> Oid.zero
                  | Some obj' -> obj' in
              if not strict || Oid.(obj' = zero || obj' = obj)
              then intern_symbol ~package sym.name obj cls
              else
                let sym' = Map.find_exn cls.syms obj' in
                Knowledge.fail (Import (sym,sym')))

    let package_exists package = Map.exists ~f:(fun {Env.objs} ->
        Map.mem objs package)

    let name_exists {package; name} = Map.exists ~f:(fun {Env.objs} ->
        match Map.find objs package with
        | None -> false
        | Some names -> Map.mem names name)


    exception Not_a_package of string [@@deriving sexp_of]
    exception Not_a_symbol of fullname [@@deriving sexp_of]

    let check_name classes = function
      | `Pkg name -> if package_exists name classes
        then Knowledge.return ()
        else Knowledge.fail (Not_a_package name)
      | `Sym sym -> if name_exists sym classes
        then Knowledge.return ()
        else Knowledge.fail (Not_a_symbol sym)

    let current = function
      | Some p -> Knowledge.return p
      | None -> gets (fun s -> s.package)

    let import ?(strict=false) ?package imports : unit knowledge =
      current package >>| escaped >>= fun package ->
      get () >>= fun s ->
      Knowledge.List.fold ~init:s.classes imports ~f:(fun classes name ->
          let name = match find_separator name with
            | None -> `Pkg name
            | Some _ -> `Sym (split_name package name) in
          let needs_import {Env.pubs} sym obj = match name with
            | `Sym s -> sym = s
            | `Pkg p -> match Map.find pubs p with
              | None -> false
              | Some pubs -> Set.mem pubs obj in
          check_name classes name >>= fun () ->
          Map.to_sequence classes |>
          Knowledge.Seq.fold ~init:classes
            ~f:(fun classes (clsid,objects) ->
                import_class ~strict ~package ~needs_import objects
                >>| fun objects ->
                Map.set classes clsid objects))
      >>= fun classes -> put {s with classes}
  end


  module Data = struct
    type +'a t = 'a obj
    type 'a ord = Oid.comparator_witness

    let atom _ x = Knowledge.return x

    let add_cell {Class.id} objects oid cell =
      let {Env.data; heap} = objects in
      let data = Map.add_exn data ~key:cell ~data:oid in
      let heap = Map.add_exn heap ~key:oid ~data:cell in
      update (fun s -> {
            s with classes = Map.set s.classes id {
          objects with data; heap
        }}) >>| fun () ->
      oid

    let cons cls car cdr =
      let cell = {car; cdr} in
      objects cls >>= function {data; heap} as s ->
      match Map.find data cell with
      | Some id -> Knowledge.return id
      | None -> match Map.max_elt heap with
        | None ->
          add_cell cls s Oid.first_cell cell
        | Some (id,_) ->
          add_cell cls s (Oid.next id) cell

    let case cls x ~null ~atom ~cons =
      if Oid.is_null x then null else
      if Oid.is_atom x || Oid.is_number x then atom x
      else objects cls >>= fun {Env.heap} ->
        let cell = Map.find_exn heap x in
        cons cell.car cell.cdr

    let id = Object.id

    module type S = Object.S
    let derive = Object.derive
  end

  module Syntax = struct
    include Knowledge.Syntax
    let (-->) x p = collect p x
    let (<--) p f = promise p f
    let (//) c s = Object.read c s
  end

  module type S = sig
    include Monad.S with type 'a t = 'a knowledge
                     and module Syntax := Syntax
    include Monad.Fail.S with type 'a t := 'a knowledge
                          and type 'a error = conflict
  end
  include (Knowledge : S)


  let compute_value cls obj =
    Slot.enum cls |> List.iter ~f:(fun (Slot.Pack s) ->
        collect s obj >>= fun v ->
        provide s obj v)


  let get_value cls obj =
    compute_value cls obj >>= fun () ->
    objects cls >>| fun {Env.vals} ->
    match Map.find vals obj with
    | None -> Value.empty cls
    | Some x -> Value.create cls x

  let run cls obj s =
    match State.run (obj >>= get_value cls) s with
    | Ok x,s -> Ok (x,s)
    | Error err,_ -> Error err


  module Conflict = Conflict
  module Agent = Agent
  type 'a opinions = 'a Opinions.t
  type agent = Agent.t
  let sexp_of_conflict = Conflict.sexp_of_t
end

type 'a knowledge = 'a Knowledge.t
