open Core_kernel.Std
open Bap_future.Std
open Bap.Std

include Self()

let width_of_mem m = Word.bitwidth (Memory.min_addr m)

let get mem data =
  let min = Memory.min_addr mem in
  let max = Memory.max_addr mem in
  Rel_fact.find min max data

let nullify_call exts mem =
  let width = width_of_mem mem in
  Option.map (get mem exts) ~f:(fun _ -> Addr.zero width)

let affect_cf insn =
  Insn.(may affect_control_flow (of_basic insn))

let contains_relocations fact mem =
  Option.is_some (get mem @@ Rel_fact.internals fact) ||
  Option.is_some (get mem @@ Rel_fact.externals fact)

let has_relocations rels mem insn =
  affect_cf insn && contains_relocations rels mem

let resolve fact mem default =
  match get mem (Rel_fact.internals fact) with
  | Some a -> a
  | None ->
    Option.value_map ~default ~f:ident
      (nullify_call (Rel_fact.externals fact) mem)

let resolve_jumps mem rels dests =
  List.map ~f:(function
      | Some addr, `Jump -> Some (resolve rels mem addr), `Jump
      | x -> x) dests

let resolve_dests b rels mem insn =
  let dests = Brancher.resolve b mem insn in
  if has_relocations rels mem insn then
    let has_fall = List.exists ~f:(fun (_,x) -> x = `Fall) dests in
    let dests = resolve_jumps mem rels dests in
    if has_fall then dests
    else
      let fall = Some (Addr.succ @@ Memory.max_addr mem), `Fall in
      fall :: resolve_jumps mem rels dests
  else dests

let create arch spec =
  let b = Brancher.of_bil arch in
  match Rel_fact.create spec with
  | Ok fact ->
    Ok (Brancher.create (resolve_dests b fact))
  | Error er ->
    error "%a" Error.pp er;
    Ok b

let init () =
  let open Project.Info in
  Stream.Variadic.(apply (args arch $ spec) ~f:create) |>
  Brancher.Factory.register "relocatable"
