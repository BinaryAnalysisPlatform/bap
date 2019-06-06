open Core_kernel
open Monads.Std

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

  let make_join name inspect order x y = match order x y with
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

  let total ?inspect ~empty ~order name =
    define ?inspect ~empty name ~order:(partial_of_total order)

  let flat ?inspect ~empty ~equal name =
    define ?inspect ~empty name ~order:(fun x y ->
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

  let optional ?(inspect=sexp_of_opaque) ~equal name =
    let inspect = sexp_of_option inspect in
    flat ~inspect ~empty:None ~equal:(Option.equal equal) name

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
  type top = Nothing.t
  type +'a abstract = Abstract : 'a abstract

  type +'a t = {
    id : Cid.t;
    name : fullname;
    data : 'a;
  }

  let classes = ref Cid.zero

  let newclass ?desc ?package name data =
    Cid.incr classes;
    {
      id = !classes;
      name = Registry.add_class ?desc ?package name;
      data;
    }

  let declare
    : ?desc:string -> ?package:string -> string -> 'a -> 'a t =
    fun ?desc ?package name data ->
    newclass ?desc ?package name data

  let abstract
    : ?desc:string -> ?package:string -> string -> 'a t =
    fun ?desc ?package name ->
    newclass ?desc ?package name Abstract

  let refine {id; name; data = Abstract} data = {id; name; data}
  let forget cls = {cls with data = Abstract}

  let same x y = Cid.equal x.id y.id

  let equal : type a b. a t -> b t -> (a obj,b obj) Type_equal.t option =
    fun x y -> Option.some_if (same x y) Type_equal.T

  let assert_equal x y = match equal x y with
    | Some t -> t
    | None ->
      failwithf "assert_equal: wrong assertion, classes of %s and %s \
                 are different"
        (string_of_fname x.name)
        (string_of_fname y.name)
        ()



  let data = fun {data} -> data
  let name {name={name}} = name
  let package {name={package}} = package
  let fullname {name} = string_of_fname name

end

module Record = struct
  module Dict = Univ_map
  type  t = Dict.t
  module Repr = struct
    type entry = {
      name : string;
      data : string;
    } [@@deriving bin_io]

    type t = entry list [@@deriving bin_io]
  end

  type univ = Dict.Packed.t


  type slot_io = {
    reader : string -> univ;
    writer : univ -> string;
  }

  type slot_domain = {
    inspect : univ -> Sexp.t;
    order : univ -> univ -> Order.partial;
    join : univ -> univ -> (univ,conflict) result;
    empty  : univ;
  }

  let io : slot_io Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let domains : slot_domain Hashtbl.M(String).t =
    Hashtbl.create (module String)

  let empty = Dict.empty

  let (<:=) x y =
    Dict.to_alist x |> List.for_all ~f:(fun (Dict.Packed.T (k,_) as x) ->
        match Dict.find y k with
        | None -> false
        | Some y ->
          let dom = Hashtbl.find_exn domains (Dict.Key.name k) in
          match dom.order x (Dict.Packed.T (k,y)) with
          | LT | EQ -> true
          | GT | NC -> false)

  let order : t -> t -> Order.partial = fun x y ->
    match x <:= y, y <:= x with
    | true,false  -> LT
    | true,true   -> EQ
    | false,true  -> GT
    | false,false -> NC

  let eq = Type_equal.Id.same_witness_exn

  let commit (type p) (key : p Dict.Key.t) v x =
    match Dict.find v key with
    | None -> Ok (Dict.set v key x)
    | Some y ->
      let dom = Hashtbl.find_exn domains (Dict.Key.name key) in
      let pack v = Dict.Packed.T (key,v) in
      match dom.join (pack y) (pack x) with
      | Ok (Dict.Packed.T (k,x)) ->
        let Type_equal.T = eq key k in
        Ok (Dict.set v key x)
      | Error err -> Error err

  let put key v x = Dict.set v key x
  let get key {Domain.empty} data = match Dict.find data key with
    | None -> empty
    | Some x -> x

  let find key v = Dict.find v key


  exception Merge_conflict of conflict

  let merge ~on_conflict x y =
    Dict.to_alist x |>
    List.fold ~init:y ~f:(fun v (Dict.Packed.T (k,x)) ->
        Dict.change v k ~f:(function
            | None -> Some x
            | Some y ->
              let name = Dict.Key.name k in
              let pack v = Dict.Packed.T (k,v) in
              let dom = Hashtbl.find_exn domains name in
              match dom.join (pack x) (pack y) with
              | Ok (Dict.Packed.T (k',x)) ->
                let Type_equal.T = eq k k' in
                Some x
              | Error err -> match on_conflict with
                | `drop_both -> None
                | `drop_left -> Some y
                | `drop_right -> Some x
                | `fail ->
                  raise (Merge_conflict err)))

  let join x y =
    try Ok (merge ~on_conflict:`fail x y)
    with Merge_conflict err -> Error err

  let register_persistent (type p)
      (key : p Dict.Key.t)
      (p : p Persistent.t) =
    let slot = Dict.Key.name key in
    let writer (Dict.Packed.T (k,x)) =
      match Type_equal.Id.same_witness k key with
      | Some Type_equal.T -> Persistent.to_string p x
      | None -> assert false in
    let reader s =
      Dict.Packed.T (key,Persistent.of_string p s) in
    Hashtbl.add_exn io ~key:slot ~data:{reader;writer}

  include Binable.Of_binable(Repr)(struct
      type t = Dict.t
      let to_binable s =
        Dict.to_alist s |>
        List.rev_filter_map ~f:(fun (Dict.Packed.T(k,_) as x) ->
            let name = Dict.Key.name k in
            match Hashtbl.find io name with
            | None -> None
            | Some {writer=to_string} ->
              Some Repr.{name; data = to_string x;})

      let of_binable entries =
        List.fold entries ~init:Dict.empty ~f:(fun s {Repr.name; data} ->
            match Hashtbl.find io name with
            | None -> s
            | Some {reader=parse} ->
              let Dict.Packed.T (key,data) = parse data in
              Dict.set s key data)
    end)

  let register_domain
    : type p. p Dict.Key.t -> p Domain.t -> unit =
    fun key dom ->
    let name = Dict.Key.name key in
    let order (Dict.Packed.T (kx,x)) (Dict.Packed.T (ky,y)) =
      let Type_equal.T = eq kx ky in
      let Type_equal.T = eq kx key in
      dom.order x y in
    let empty = Dict.Packed.T(key, dom.empty) in
    let inspect (Dict.Packed.T(kx,x)) =
      let Type_equal.T = eq kx key in
      dom.inspect x in
    let join (Dict.Packed.T (kx,x)) (Dict.Packed.T (ky,y)) =
      let Type_equal.T = eq kx key in
      let Type_equal.T = eq ky key in
      match dom.join x y with
      | Ok x -> Ok (Dict.Packed.T (key, x))
      | Error err -> Error err in
    Hashtbl.add_exn domains ~key:name ~data:{
      inspect;
      empty;
      order;
      join;
    }

  let sexp_of_t x =
    Sexp.List (Dict.to_alist x |> List.map ~f:(function
          Dict.Packed.T (k,x) ->
          let name = Dict.Key.name k in
          let dom = Hashtbl.find_exn domains name in
          Sexp.List [
            Atom name;
            dom.inspect (Dict.Packed.T(k,x));
          ]))

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
    cls  : 'a Class.t;
    data : Record.t;
    time : Int63.t;
  }
  type 'a cls = 'a Class.t
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
      comp : work Map.M(Uid).t Map.M(Oid).t;
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
      cls : 'a Class.t;
      dom : 'p Domain.t;
      key : 'p Type_equal.Id.t;
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
      let key = Type_equal.Id.create ~name dom.inspect in
      Option.iter persistent (Record.register_persistent key);
      Record.register_domain key dom;
      let promises = Hashtbl.create (module Pid) in
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

    let clone cls {data; time} = {cls; data; time}
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
      {
        x with time = next_second ();
               data = Record.merge ~on_conflict x.data y.data
      }


    let join x y = match Record.join x.data y.data with
      | Ok data -> Ok {x with data; time = next_second ()}
      | Error c -> Error c

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
      : type a. a cls ->
        (module S with type t = a t and type comparator_witness = a ord) =
      fun cls ->
      let module R = struct
        type t = a value
        let sexp_of_t x = Record.sexp_of_t x.data
        let t_of_sexp = opaque_of_sexp
        let empty = empty cls

        include Binable.Of_binable(Record)(struct
            type t = a value
            let to_binable : 'a value -> Record.t =
              fun {data} -> data
            let of_binable : Record.t -> 'a value =
              fun data -> {cls; data; time = next_second ()}
          end)
        type comparator_witness = Comparator.comparator_witness
        include Base.Comparable.Make_using_comparator(struct
            type t = a value
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
    let strip
      : type a b. (a t, b t) Type_equal.t -> (a,b) Type_equal.t =
      fun T -> T
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
            | Some v -> match Record.commit slot.key v x with
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

  let objects : _ cls -> _ = fun cls ->
    get () >>| fun {classes} ->
    match Map.find classes cls.id with
    | None -> Env.empty_class
    | Some objs -> objs

  let uid {Slot.key} = Type_equal.Id.uid key

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
          | None -> Map.empty (module Uid)
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

    let create : 'a cls -> 'a obj Knowledge.t = fun cls ->
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

    let derive : type a. a cls ->
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
