open Core_kernel
open Monads.Std

module Order = struct
  type partial = LT | EQ | GT | NC
  module type S = sig
    type t
    val order : t -> t -> partial
  end
end

type conflict = ..

module Conflict = struct
  type t = conflict = ..
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


module Domain = struct
  type 'a t = {
    inspect : 'a -> Sexp.t;
    empty : 'a;
    order : 'a -> 'a -> Order.partial;
    name : string;
  }

  let define ?(inspect=sexp_of_opaque) ~empty ~order name = {
    inspect; empty; order; name;
  }

  let partial_of_total order x y : Order.partial = match order x y with
    | 0 -> EQ
    | 1 -> GT
    | _ -> LT

  let total ?inspect ~empty ~order name =
    define ?inspect ~empty name ~order:(partial_of_total order)

  let flat ?inspect ~empty ~is_empty name =
    define ?inspect ~empty name ~order:(fun x y ->
        match is_empty x, is_empty y with
        | true,true -> EQ
        | true,false -> LT
        | false,true -> GT
        | false,false -> NC)


  let mapping (type k) (type o) ?(equal=(fun _ _ -> true))
      (module K : Comparator.S with type t = k
                                and type comparator_witness = o)
      name =
    let empty = Map.empty (module K) in
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
    define ~inspect ~empty ~order name

  let optional ?inspect name =
    let inspect = match inspect with
      | None -> None
      | Some sexp_of_elt -> Some (sexp_of_option sexp_of_elt) in
    flat ?inspect ~empty:None ~is_empty:Option.is_none name

  let string = define "string" ~empty:""
      ~inspect:sexp_of_string ~order:(fun x y ->
          match String.is_empty x, String.is_empty y with
          | true, true -> EQ
          | true,false -> GT
          | false,true -> LT
          | false,false -> partial_of_total String.compare x y)
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
}

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
    order  : univ -> univ -> Order.partial;
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


  let put key v x = Dict.set v key x
  let get key {Domain.empty} data = match Dict.find data key with
    | None -> empty
    | Some x -> x

  exception Merge_conflict of string * Sexp.t * Sexp.t

  type conflict += Merge of string * Sexp.t * Sexp.t

  let merge ~on_conflict x y =
    Dict.to_alist x |>
    List.fold ~init:y ~f:(fun v (Dict.Packed.T (k,x)) ->
        Dict.change v k ~f:(function
            | None -> Some x
            | Some y ->
              let name = Dict.Key.name k in
              let pack v = Dict.Packed.T (k,v) in
              let dom = Hashtbl.find_exn domains name in
              match dom.order (pack x) (pack y) with
              | EQ | LT -> Some x
              | GT -> Some y
              | NC -> match on_conflict with
                | `drop_both -> None
                | `drop_left -> Some y
                | `drop_right -> Some x
                | `fail ->
                  let x = dom.inspect (pack x)
                  and y = dom.inspect (pack y) in
                  raise (Merge_conflict (name,x,y))))

  let join x y =
    try Ok (merge ~on_conflict:`fail x y)
    with Merge_conflict (n,x,y) -> Error (Merge (n,x,y))

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
    let eq = Type_equal.Id.same_witness_exn in
    let order (Dict.Packed.T (kx,x)) (Dict.Packed.T (ky,y)) =
      let Type_equal.T = eq kx ky in
      let Type_equal.T = eq kx key in
      dom.order x y in
    let empty = Dict.Packed.T(key, dom.empty) in
    let inspect (Dict.Packed.T(kx,x)) =
      let Type_equal.T = eq kx key in
      dom.inspect x in
    Hashtbl.add_exn domains ~key:name ~data:{
      inspect;
      empty;
      order;
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
    type objects = {
      vals : Record.t Map.M(Oid).t;
      reqs : Set.M(Oid).t Map.M(Pid).t;
      objs : Oid.t Map.M(String).t Map.M(String).t;
      syms : fullname Map.M(Oid).t;
      pubs : Set.M(Oid).t Map.M(String).t;
      heap : cell Map.M(Oid).t;
      data : Oid.t Map.M(Cell).t;
    }

    let empty_class = {
      vals = Map.empty (module Oid);
      reqs = Map.empty (module Pid);
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
      get : Oid.t -> 'p Knowledge.t;
      pid : pid;
    }

    type (+'a,'p) t = {
      cls : 'a Class.t;
      dom : 'p Domain.t;
      key : 'p Type_equal.Id.t;
      name : string;
      desc : string option;
      mutable promises : 'p promise list;
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
      let slot = {cls; dom; key; name; desc; promises = []} in
      register slot;
      slot
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
        let domain = Domain.flat ~empty ~is_empty:(equal empty)
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

  let provide : type a p. (a,p) slot -> a obj -> p -> unit Knowledge.t =
    fun slot obj x -> update @@ function {classes} as s ->
        let {Env.vals} as objs =
          match Map.find classes slot.cls.id with
          | None -> Env.empty_class
          | Some objs -> objs in {
          s with classes = Map.set classes ~key:slot.cls.id ~data:{
            objs with vals = Map.update vals obj ~f:(function
            | None -> Record.(put slot.key empty x)
            | Some v -> Record.put slot.key v x)
          }
        }

  let pids = ref Pid.zero

  let promise (s : _ slot) get =
    Pid.incr pids;
    let pid = !pids in
    s.promises <- {get;pid} :: s.promises

  let is_active reqs pid id = match Map.find reqs pid with
    | None -> false
    | Some ids -> Set.mem ids id

  let activate reqs pid id = Map.update reqs pid ~f:(function
      | None -> Set.singleton (module Oid) id
      | Some ids -> Set.add ids id)

  let deactivate req pid id = Map.change req pid ~f:(function
      | None -> None
      | Some ids ->
        let ids = Set.remove ids id in
        if Set.is_empty ids then None else Some ids)

  let objects : _ cls -> _ = fun cls ->
    get () >>| fun {classes} ->
    match Map.find classes cls.id with
    | None -> Env.empty_class
    | Some objs -> objs

  let update_reqs : _ slot -> _ = fun slot reqs ->
    update @@ function {classes} as s -> {
        s with classes = Map.update classes slot.cls.id ~f:(function
        | None -> {Env.empty_class with reqs}
        | Some objs -> {objs with reqs})
      }

  let collect : type a p. (a,p) slot -> a obj -> p Knowledge.t =
    fun slot id ->
    objects slot.cls >>= fun {Env.vals} ->
    let init = match Map.find vals id with
      | None -> slot.dom.empty
      | Some v -> Record.get slot.key slot.dom v in
    slot.promises |>
    Knowledge.List.fold ~init ~f:(fun curr req ->
        objects slot.cls >>= fun {Env.reqs} ->
        if is_active reqs req.pid id
        then Knowledge.return curr
        else
          update_reqs slot (activate reqs req.pid id) >>= fun () ->
          req.get id >>= fun next ->
          update_reqs slot (deactivate reqs req.pid id) >>= fun () ->
          match slot.dom.order curr next with
          | GT -> Knowledge.return curr
          | LT | EQ | NC -> Knowledge.return next) >>= fun max ->
    provide slot id max >>| fun () -> max

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

    let delete {Class.id} obj =
      update @@ function {classes} as s -> {
          s with
          classes = Map.change classes id ~f:(function
              | None -> None
              | Some objs ->
                let vals = Map.remove objs.vals obj in
                if Map.is_empty vals then None
                else Some {objs with vals})
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
        Scanf.sscanf input "#<%s %s>" @@ fun _ obj ->
        Knowledge.return (Oid.of_string obj)
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


    type conflict += Import of fullname * fullname

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


    type conflict += Not_a_package of string
    type conflict += Not_a_symbol of fullname

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

end

type 'a knowledge = 'a Knowledge.t
