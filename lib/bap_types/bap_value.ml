open Bap_core_theory
open Core_kernel
open Regular.Std
open Format

module type S = sig
  type t [@@deriving bin_io, compare, sexp]
  val pp : Format.formatter -> t -> unit
end

module Uid = Type_equal.Id.Uid
module Typeid = String

type void
type literal = (void,void,void) format
type uid = Uid.t
type typeid = Typeid.t [@@deriving bin_io, compare, sexp]

type 'a tag = {
  key : 'a Type_equal.Id.t;
  slot : (Theory.program,'a option) KB.slot;
}

module Value = struct
  type t = Univ_map.Packed.t = T : 'a Type_equal.Id.t * 'a -> t
end

module Equal = struct
  type ('a,'b) t = ('a,'b) Type_equal.t = T : ('a,'a) t
  let proof = Type_equal.Id.same_witness_exn
  let try_prove = Type_equal.Id.same_witness
end

type type_info = {
  pp   : Format.formatter -> Value.t -> unit;
  of_string : string -> Value.t;
  to_string : Value.t -> string;
  of_sexp : Sexp.t -> Value.t;
  to_sexp : Value.t -> Sexp.t;
  collect : Theory.Label.t -> Univ_map.t -> Univ_map.t KB.t;
  compare : Value.t -> Value.t -> int;
}

let names : (string, string) Hashtbl.t = Hashtbl.create (module String)
let types : (typeid, type_info) Hashtbl.t  =
  Hashtbl.create ~size:128 (module Typeid)

let uid = Type_equal.Id.uid

type ('a,'b) eq = ('a,'b) Type_equal.t = T : ('a,'a) eq

let register_slot (type a) slot
    (module S : S with type t = a) : a tag =
  let name = KB.Slot.fullname slot in
  let key = Type_equal.Id.create name S.sexp_of_t in
  let pp ppf (Value.T (k,x)) =
    let T = Equal.proof k key in
    S.pp ppf x in
  let of_string str =
    Value.T (key, Binable.of_string (module S) str) in
  let to_string (Value.T (k,x)) =
    let T = Equal.proof k key in
    Binable.to_string (module S) x in
  let of_sexp str =
    Value.T (key, S.t_of_sexp str) in
  let to_sexp (Value.T (k,x)) =
    let T = Equal.proof k key in
    S.sexp_of_t x in
  let compare (Value.T (kx,x)) (Value.T (ky,y)) =
    match Equal.try_prove kx ky with
    | None -> Uid.compare (uid kx) (uid ky)
    | Some T ->
      let T = Equal.proof kx key in
      S.compare x y in
  let collect obj dict =
    let open KB.Syntax in
    KB.collect slot obj >>| function
    | None -> dict
    | Some x -> Univ_map.set dict key x in
  let info = {
    pp;
    of_sexp;
    to_sexp;
    of_string;
    to_string;
    collect;
    compare;
    } in
  Hashtbl.add_exn types ~key:name ~data:info;
  Hashtbl.add_exn names ~key:name ~data:(KB.Slot.name slot);
  {key; slot}

let register (type a) ~name ~uuid (module S : S with type t = a) =
  let persistent = KB.Persistent.of_binable (module struct
      type t = S.t option [@@deriving bin_io]
    end) in
  let equal x y = S.compare x y = 0 in
  let domain = KB.Domain.optional ~equal name in
  let slot = KB.Class.property ~persistent ~package:uuid
      Theory.Program.cls name domain in
  register_slot slot (module S)

let key_name k = Hashtbl.find_exn names (Type_equal.Id.name k)
let key_typeid k = Type_equal.Id.name k

let tagname (Value.T (k,_)) = key_name k

let typeid (Value.T (k,_)) = key_typeid k

let info typeid =
  Hashtbl.find_and_call types typeid
    ~if_found:ident
    ~if_not_found:(fun typeid ->
        invalid_argf "Can't deserialize type %s, \
                      as it is no longer known to the system"
          typeid ())

let ops x = info (typeid x)
let compare_value x y = (ops x).compare x y
let compare = compare_value
let sexp_of_value x = Sexp.List [
    Sexp.Atom (typeid x);
    (ops x).to_sexp x;
  ]

let value_of_sexp = function
  | Sexp.List [Atom typeid; repr] ->
    (info typeid).of_sexp repr
  | _ -> invalid_arg "Value.t_of_sexp: broken representation"


module Univ = struct
  type t = Value.t
  let sexp_of_t = sexp_of_value
  let t_of_sexp = value_of_sexp
  let compare = compare_value
  module Repr = struct
    type t = {
      typeid : string;
      data : string;
    } [@@deriving bin_io]
  end

  include Binable.Of_binable(Repr)(struct
      type t = Value.t
      let to_binable x = Repr.{
          typeid = typeid x;
          data = (ops x).to_string x;
        }
      let of_binable {Repr.typeid; data} =
        (info typeid).of_string data
    end)
end

let create  {key} x = Value.T (key,x)
let is {key} (Value.T (k,_)) = Type_equal.Id.same key k

let get
  : type a. a tag -> Value.t -> a option =
  fun {key} (Value.T (k,x)) ->
  if Type_equal.Id.same key k
  then
    let T = Equal.proof key k in
    Some x
  else None

let get_exn
  : type a. a tag -> Value.t -> a =
  fun {key} (Value.T (k,x)) ->
  let T = Equal.proof key k in
  x


module Tag = struct
  type 'a t = 'a tag
  let name tag = key_name tag.key
  let typeid tag = key_typeid tag.key
  let key tag = tag.key
  let uid tag = uid tag.key

  let register (type a) ~name ~uuid
      (typ : (module S with type t = a)) : a tag =
    register ~name ~uuid typ

  let register_slot slot ops = register_slot slot ops
  let slot tag = tag.slot

  let same_witness t1 t2 =
    Option.try_with (fun () ->
        Type_equal.Id.same_witness_exn t1.key t2.key)

  let same_witness_exn t1 t2 =
    Type_equal.Id.same_witness_exn t1.key t2.key

  let same t1 t2 = Type_equal.Id.same t1.key t2.key
end

module Match = struct

  type 's t = {
    default : (unit -> 's);
    handlers : (Value.t -> 's) Map.M(Uid).t;
  }

  let empty = Map.empty (module Uid)

  let default default = {
    handlers = empty;
    default = default;
  }

  let case t f (tab : 's t) =
    let h = Map.set tab.handlers (Tag.uid t) (fun v -> f (get_exn t v)) in
    {tab with handlers = h}

  let run (Value.T (k,_) as v) tab =
    match Map.find tab.handlers (uid k) with
    | Some f -> f v
    | None -> tab.default ()
  let switch = run
  let select x y = switch y x
end


module Dict = struct
  type t = Univ_map.t
  let empty = Univ_map.empty
  let is_empty = Univ_map.is_empty
  let set dict {key} x = Univ_map.set dict key x
  let remove dict {key} = Univ_map.remove dict key
  let mem dict {key} = Univ_map.mem dict key
  let find dict {key} = Univ_map.find dict key
  let add dict {key} x = Univ_map.add dict key x
  let change dict {key} f = Univ_map.change dict key ~f
  let data dict =
    Univ_map.to_alist dict |>
    Seq.of_list
  let to_sequence dict =
    Seq.map (data dict) ~f:(fun v -> typeid v,v)
  let filter t ~f =
    data t |>
    Seq.fold ~init:empty ~f:(fun dict (Value.T (k,x) as v) ->
        if f v then Univ_map.set dict k x else dict)

  let compare x y =
    compare_list
      compare_value
      (Univ_map.to_alist x)
      (Univ_map.to_alist y)

  module Data = struct
    type t = Univ.t list [@@deriving bin_io, sexp]
    let of_dict = Univ_map.to_alist
    let to_dict =
      List.fold ~init:empty ~f:(fun dict (Value.T (k,x)) ->
          Univ_map.set dict k x)
  end
  include Binable.Of_binable(Data)(struct
      type t = Univ_map.t
      let to_binable = Data.of_dict
      let of_binable = Data.to_dict
    end)
  include Sexpable.Of_sexpable(Data)(struct
      type t = Univ_map.t
      let to_sexpable = Data.of_dict
      let of_sexpable = Data.to_dict
    end)
end

type dict = Dict.t [@@deriving bin_io, compare, sexp]
type t = Univ.t [@@deriving bin_io, compare, sexp]
include struct type value = Univ.t [@@deriving bin_io] end
include Regular.Make(struct
    type t = Univ.t [@@deriving bin_io, compare, sexp]
    let compare = Univ.compare
    let hash = Hashtbl.hash
    let pp ppf v = (ops v).pp ppf v
    let module_name = Some "Bap.Std.Value"
    let version = "2.0.0"
  end)
