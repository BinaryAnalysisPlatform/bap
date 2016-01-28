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

module type Tag = sig
  type t
  val tag : t tag
end

module Typeid = Bap_uuid

type typeid = Typeid.t with bin_io, compare, sexp

type type_info = {
  uuid : Typeid.t;
  pp   : Format.formatter -> Univ.t -> unit;
  of_string : string -> Univ.t;
  to_string : Univ.t -> string;
  compare : Univ.t -> Univ.t -> int;
  witness : (module S);
  tag : (module Tag);
}

let types : type_info Type_equal.Id.Uid.Table.t  =
  Type_equal.Id.Uid.Table.create ~size:128 ()

let info_of_uuid uuid =
  Hashtbl.to_alist types |> List.find ~f:(fun (k,i) -> i.uuid = uuid)

let unpack_tag (type a) (module T : Tag with type t = a) = T.tag

external unsafe_cast : (module Tag) -> (module Tag with type t = 'a) = "%identity"

let same_modules (module X : S) (module Y : S) =
  phys_same X.compare Y.compare

let tag_of_info (type a) (module S : S with type t = a) info
  : a tag option =
  if same_modules info.witness (module S)
  then Some (unpack_tag (unsafe_cast info.tag))
  else None

let register (type a) ~name ~uuid
    (typ : (module S with type t = a)) : a tag =
  let module S = (val typ) in
  match info_of_uuid uuid with
  | None ->
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
      witness = (module S);
      tag = (module (struct type t = a let tag = tag end));
    } in
    Hashtbl.add_exn types ~key:(Type_equal.Id.uid tag) ~data:info;
    tag
  | Some (_,info) -> match tag_of_info typ info with
    | None ->
      invalid_argf "UUID %s is already in use" (Typeid.to_string uuid) ()
    | Some tag -> tag


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

module Match = struct

  module Map = Type_equal.Id.Uid.Map

  type 's t = {
    default : (unit -> 's);
    handlers : (value -> 's) Map.t;
  }

  let empty = Map.empty

  let default default = {
    handlers = empty;
    default = default;
  }

  let case t f (tab : 's t) =
    let handlers = Map.add tab.handlers (Type_equal.Id.uid t)
        (fun v -> f (get_exn t v)) in
    {tab with handlers}

  let run v tab =
    match Map.find tab.handlers (Univ.type_id_uid v) with
    | Some f -> f v
    | None -> tab.default ()

  let switch = run

  let select x y = switch y x
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

  let all_pairs t : (typeid * value) Sequence.t =
    Map.to_sequence t
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
    let version = "0.1"

  end)
