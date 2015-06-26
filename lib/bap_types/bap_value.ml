open Core_kernel.Std
open Bap_common

module Vector = Bap_vector

type void
type literal = (void,void,void) format

module type S = sig
  type t with bin_io, compare, sexp
  val pp : Format.formatter -> t -> unit
end

type 'a tag = 'a Type_equal.Id.t
type t = Univ.t

module Typeid = struct

  module Bin = Bin_prot.Utils.Make_binable(struct
      module Binable = String
      type t = Uuidm.t
      let to_binable = Uuidm.to_bytes
      let of_binable s = match Uuidm.of_bytes s with
        | None -> invalid_arg "Bad UUID format"
        | Some uuid -> uuid
    end)

  module Stringable = struct
    type t = Uuidm.t
    let of_string s = match Uuidm.of_string s with
      | None -> invalid_arg "Bad UUID format"
      | Some uuid -> uuid
    let to_string s = Uuidm.to_string s
  end

  module Sexp = Sexpable.Of_stringable(Stringable)

  include Regular.Make(struct
      let compare = Uuidm.compare
      include Bin
      include Sexp
      include Stringable
      let hash = Hashtbl.hash
      let module_name = Some "Bap.Std.Value.Typeid"
      let pp ppf t = Uuidm.print ppf t
    end)
  include Uuidm
  let of_string = Stringable.of_string
  let to_string = Stringable.to_string
end

type typeid = Typeid.t with bin_io, compare, sexp


type type_info = {
  uuid : Typeid.t;
  pp   : Format.formatter -> Univ.t -> unit;
  of_string : string -> Univ.t;
  to_string : Univ.t -> string;
  compare : Univ.t -> Univ.t -> int;
}

let types : type_info Type_equal.Id.Uid.Table.t  =
  Type_equal.Id.Uid.Table.create ~size:128 ()

let info_of_uuid uuid =
  Hashtbl.to_alist types |> List.find ~f:(fun (k,i) -> i.uuid = uuid)

let register (type a) ~name ~uuid
    (typ : (module S with type t = a)) : a tag =
  let module S = (val typ) in
  if info_of_uuid uuid <> None then
    invalid_argf "UUID %s is already in use" (Uuidm.to_string uuid) ();
  let tag = Type_equal.Id.create name S.sexp_of_t in
  let pp ppf univ = S.pp ppf (Univ.match_exn univ tag) in
  let of_string str =
    Univ.create tag (Binable.of_string (module S) str) in
  let to_string x =
    Binable.to_string (module S) (Univ.match_exn x tag) in
  let compare x y = match Univ.match_ x tag, Univ.match_ y tag with
    | Some x, Some y -> S.compare x y
    | _,_ -> Type_equal.Id.Uid.compare
               (Univ.type_id_uid x) (Univ.type_id_uid y) in
  let info = {
    uuid; pp;
    of_string;
    to_string;
    compare;
  } in
  Hashtbl.add_exn types ~key:(Type_equal.Id.uid tag) ~data:info;
  tag

module Nil = struct
  type t = Typeid.t * string with sexp_of
  let t = Type_equal.Id.create "nil" sexp_of_t
end

let typeof v = Hashtbl.find types (Univ.type_id_uid v)

include Bin_prot.Utils.Make_binable(struct
    module Binable = struct
      type t = Typeid.t * string with bin_io
    end
    type nonrec t = t
    let of_binable (uuid,data) = match info_of_uuid uuid with
      | Some (_,t) -> t.of_string data
      | None -> Univ.create Nil.t (uuid,data)

    let to_binable v = match typeof v with
      | Some t -> t.uuid, t.to_string v
      | None -> Univ.match_exn v Nil.t
  end)

include Sexpable.Of_sexpable
    (struct type t = Typeid.t * string with sexp end)
    (struct
      type nonrec t = t

      let of_sexpable (uuid,data) = match info_of_uuid uuid with
        | Some (_,t) -> t.of_string data
        | None -> Univ.create Nil.t (uuid,data)

      let to_sexpable v = match typeof v with
        | Some t -> t.uuid, t.to_string v
        | None -> Univ.match_exn v Nil.t
    end)

type value = t with bin_io, sexp

let compare_value x y =
  match typeof x, typeof y with
  | None,_ | _,None -> Pervasives.compare x y
  | Some t,_ -> t.compare x y


let compare = compare_value

let create = Univ.create
let get t x = Univ.match_ x t
let get_exn t x = Univ.match_exn x t
let is t x = Univ.does_match x t
let tagname t = Univ.type_id_name t
let typeid x = match typeof x with
  | Some t -> t.uuid
  | None -> fst (Univ.match_exn x Nil.t)

module Tag = struct
  type 'a t = 'a tag
  let name = Type_equal.Id.name

  let register (type a) ~name ~uuid
      (typ : (module S with type t = a)) : a tag =
    let uuid = Typeid.of_string (string_of_format uuid) in
    register ~name:(string_of_format name) ~uuid typ



  (* core changed the type of same_witness in transition from 111 to
     112, so we can't use more efficient [same_witness]. When we drop,
     4.01 support we can rewrite it to more efficient functions.  *)
  let same_witness t1 t2 =
    Option.try_with (fun () -> Type_equal.Id.same_witness_exn t1 t2)

  let same_witness_exn = Type_equal.Id.same_witness_exn
  let same = Type_equal.Id.same

end

module Dict = struct

  type t = value Typeid.Map.t with bin_io, compare, sexp

  let uuid tag =
    (Hashtbl.find_exn types (Type_equal.Id.uid tag)).uuid

  let empty = Typeid.Map.empty
  let set t key data = Map.add t ~key:(uuid key) ~data:(create key data)
  let mem t key = Map.mem t (uuid key)
  let remove t key = Map.remove t (uuid key)
  let is_empty = Map.is_empty
  let find t key = match Map.find t (uuid key) with
    | None -> None
    | Some x -> get key x
  let add t key data =
    if mem t key then `Duplicate else `Ok (set t key data)
  let change t key update =
    let orig = find t key in
    let next = update orig in
    match next with
    | Some data -> set t key data
    | None -> if Option.is_none orig then t else remove t key

  let data t : value Sequence.t =
    Map.to_sequence t |> Sequence.map ~f:snd
end

type dict = Dict.t with bin_io, compare,sexp

include Regular.Make(struct
    type nonrec t = t with bin_io, compare, sexp
    let hash = Hashtbl.hash

    let pp ppf v = match typeof v with
      | Some t -> t.pp ppf v
      | None -> Format.fprintf ppf "<poly>"
    let module_name = Some "Bap.Std.Value"
  end)
