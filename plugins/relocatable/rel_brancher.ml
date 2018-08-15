open Core_kernel.Std
open Bap_future.Std
open Bap.Std

include Self()

let affect_cf insn =
  Insn.(may affect_control_flow (of_basic insn))

let is_return insn = Insn.(is return (of_basic insn))

let find mem data =
  Rel_data.find (Memory.min_addr mem) (Memory.max_addr mem) data

let contains_relocations (rels,exts) mem =
  Option.is_some (find mem rels) ||
  Option.is_some (find mem exts)

let has_relocations rels mem insn =
  affect_cf insn && contains_relocations rels mem

let resolve (rels,exts) mem default =
  match find mem rels with
  | Some a -> Some a
  | None ->
    match find mem exts with
    | Some _ -> None
    | _ -> Some default

let resolve_jumps mem rels dests =
  List.map ~f:(function
      | Some addr, `Jump -> resolve rels mem addr, `Jump
      | x -> x) dests

let resolve_dests b rels mem insn =
  let dests = Brancher.resolve b mem insn in
  if has_relocations rels mem insn then
    let has_fall = List.exists ~f:(fun (_,x) -> x = `Fall) dests in
    let dests = resolve_jumps mem rels dests in
    if has_fall then dests
    else if is_return insn then
      let fall = Some (Addr.succ @@ Memory.max_addr mem), `Fall in
      fall :: resolve_jumps mem rels dests
    else dests
  else dests

let create arch spec =
  let b = Brancher.of_bil arch in
  match Rel_data.all spec with
  | Ok (rels, exts) ->
    Ok (Brancher.create (resolve_dests b (rels,exts)))
  | Error er ->
    error "%a" Error.pp er;
    Ok b

let init () =
  let open Project.Info in
  Stream.Variadic.(apply (args arch $ spec) ~f:create) |>
  Brancher.Factory.register "relocatable"
