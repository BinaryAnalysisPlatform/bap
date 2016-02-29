open Core_kernel.Std
open Regular.Std
open Bap_common
open Format

module Typeid = String

module type S = sig
  type t with bin_io, compare, sexp
  val pp : Format.formatter -> t -> unit
end

type void
type univ = Univ.t
type literal = (void,void,void) format
type typeid = String.t with bin_io, compare, sexp

type 'a tag = {
  tid : typeid;
  key : 'a Type_equal.Id.t;
}

type t = {
  uuid : typeid;
  data : string
} with bin_io, sexp

type value = t with bin_io, sexp

type type_info = {
  pp   : Format.formatter -> univ -> unit;
  of_string : string -> univ;
  to_string : univ -> string;
  compare : univ -> univ -> int;
}

let types : (typeid, type_info) Hashtbl.t  =
  Typeid.Table.create ~size:128 ()

let register (type a) ~name ~uuid
    (typ : (module S with type t = a)) : a tag =
  let module S = (val typ) in
  match Hashtbl.find types uuid with
  | None ->
    let uuid = match Uuidm.of_string uuid with
      | None -> invalid_arg "Invalid UUID format"
      | Some uuid -> Uuidm.to_bytes uuid in
    let key = Type_equal.Id.create name S.sexp_of_t in
    let pp ppf univ = S.pp ppf (Univ.match_exn univ key) in
    let of_string str =
      Univ.create key (Binable.of_string (module S) str) in
    let to_string x =
      Binable.to_string (module S) (Univ.match_exn x key) in
    let compare x y = match Univ.match_ x key, Univ.match_ y key with
      | Some x, Some y -> S.compare x y
      | _,_ -> Type_equal.Id.Uid.compare
                 (Univ.type_id_uid x) (Univ.type_id_uid y) in
    let info = {
      pp;
      of_string;
      to_string;
      compare;
    } in
    Hashtbl.add_exn types ~key:uuid ~data:info;
    {key; tid = uuid}
  | Some _ ->
    invalid_argf "UUID %s is already in use" (Typeid.to_string uuid) ()

let nil =
  let key = Type_equal.Id.create "nil" sexp_of_string in
  let pp ppf _ = fprintf ppf "<poly>" in
  let of_string s = Univ.create key s in
  (* to_string is it is called only in create, where it is guaranteed
     that the tag exists, in other words, to_string will be never
     called *)
  let to_string x = assert false in
  let compare x y = Pervasives.compare x y in
  let info = {
    of_string;
    to_string;
    pp; compare
  } in
  Hashtbl.add_exn types ~key:"" ~data:info;
  info

let typeof v = match Hashtbl.find types v.uuid with
  | Some info -> info
  | None -> nil

let compare_value x y =
  match compare x.uuid y.uuid with
  | 1 | -1 as r -> r
  | _ -> match typeof x, typeof y with
    | t, s -> t.compare (t.of_string x.data) (s.of_string y.data)
let compare = compare_value

let create tag x : value =
  let info = Hashtbl.find_exn types tag.tid in
  {
    uuid = tag.tid;
    data = info.to_string (Univ.create tag.key x);
  }

let univ x = (typeof x).of_string x.data
let get t x =
  if Typeid.(x.uuid = t.tid)
  then Univ.match_ (univ x) t.key
  else None
let get_exn t x = match get t x with
  | Some x -> x
  | None -> invalid_arg "Value.get_exn: wrong tag"
let is t x = Typeid.equal x.uuid t.tid
let tagname x = Univ.type_id_name (univ x)
let typeid x = x.uuid

module Tag = struct
  type 'a t = 'a tag
  let name tag = Type_equal.Id.name tag.key
  let typeid tag = tag.tid
  let key tag = tag.key

  let register (type a) ~name ~uuid
      (typ : (module S with type t = a)) : a tag =
    let uuid = Typeid.of_string (string_of_format uuid) in
    register ~name:(string_of_format name) ~uuid typ

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
    handlers : (value -> 's) Typeid.Map.t;
  }

  let empty = Typeid.Map.empty

  let default default = {
    handlers = empty;
    default = default;
  }

  let case t f (tab : 's t) =
    let h = Map.add tab.handlers t.tid (fun v -> f (get_exn t v)) in
    {tab with handlers = h}

  let run v tab =
    match Map.find tab.handlers v.uuid with
    | Some f -> f v
    | None -> tab.default ()

  let switch = run

  let select x y = switch y x
end


module Dict = struct

  type t = value Typeid.Map.t with bin_io, compare, sexp

  let uuid tag = tag.tid

  let empty = Typeid.Map.empty

  let set t tag data =
    Map.add t ~key:(uuid tag) ~data:(create tag data)

  let mem t key = Map.mem t (uuid key)
  let remove t key = Map.remove t (uuid key)
  let is_empty = Map.is_empty
  let find t tag = match Map.find t (uuid tag) with
    | None -> None
    | Some x -> get tag x
  let add t key data =
    if mem t key then `Duplicate else `Ok (set t key data)
  let change t key update =
    let orig = find t key in
    let next = update orig in
    match next with
    | Some data -> set t key data
    | None -> if Option.is_none orig then t else remove t key

  let to_sequence t : (typeid * value) Sequence.t =
    Map.to_sequence t
  let data t : value Sequence.t =
    Map.to_sequence t |> Sequence.map ~f:snd
end

type dict = Dict.t with bin_io, compare,sexp

include Regular.Make(struct
    type nonrec t = t with bin_io, compare, sexp
    let hash = Hashtbl.hash

    let pp ppf v =
      let t = typeof v in
      t.pp ppf (t.of_string v.data)
    let module_name = Some "Bap.Std.Value"
    let version = "0.2"
  end)
