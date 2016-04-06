open Core_kernel.Std
open Regular.Std
open Bap.Std
open Language.Std

type 'a target = {
  default : string option;
  resolve : 'a -> sub term -> string option;
  mappers : ('a -> sub term -> sub term) String.Map.t;
}

module Arches = Language.Property(struct
    type 'a t = 'a target Arch.Map.t
  end)

let arches = Arches.create ()

let nothing _ _ = None


let empty = {
  default = None;
  resolve = nothing;
  mappers = String.Map.empty
}

let register lang arch name f : unit =
  let empty = {empty with default = Some name} in
  let targets,target = match Arches.get arches lang with
    | None -> Arch.Map.empty,empty
    | Some targets -> targets, match Arch.Map.find targets arch with
      | Some target -> target
      | None -> empty in
  let mappers = Map.add target.mappers ~key:name ~data:f in
  let target = {target with mappers} in
  let targets = Map.add targets ~key:arch ~data:target in
  Arches.set arches lang targets

let target lang arch =
  match Arches.get arches lang with
  | None -> None
  | Some targets -> Map.find targets arch

let update_target lang arch ~f =
  let targets = match Arches.get arches lang with
    | Some targets -> targets
    | None -> Arch.Map.empty in
  Map.change targets arch ~f |>
  Arches.set arches lang

let override_default lang arch name =
  update_target lang arch ~f:(function
      | Some target -> Some {target with default = Some name}
      | None -> Some { empty with default = Some name})

let override_resolver lang arch resolve =
  update_target lang arch ~f:(function
      | Some target -> Some {target with resolve}
      | None -> Some { empty with resolve})


let apply lang arch api sub = match target lang arch with
  | None -> sub
  | Some t ->
    let abi = match t.resolve api sub with
      | None -> t.default
      | other -> other in
    match abi with
    | None -> sub
    | Some abi -> match Map.find t.mappers abi with
      | None -> sub
      | Some map -> map api sub


let known lang = match Arches.get arches lang with
  | None -> []
  | Some targets ->
    Map.to_alist targets |>
    List.map ~f:(fun (n,t) -> n,Map.keys t.mappers)
