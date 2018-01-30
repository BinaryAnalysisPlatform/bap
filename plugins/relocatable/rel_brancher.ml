open Core_kernel.Std
open Bap_future.Std
open Bap.Std

include Self()

let get mem data =
  Rel_fact.find data
    ~from:(Memory.min_addr mem) ~to_:(Memory.max_addr mem)

let affect_cf insn =
  Insn.(may affect_control_flow (of_basic insn))

let is_return insn = Insn.(is return (of_basic insn))

let contains_relocations fact mem =
  Option.is_some (get mem @@ Rel_fact.relocations fact) ||
  Option.is_some (get mem @@ Rel_fact.externals fact)

let has_relocations fact mem insn =
  affect_cf insn && contains_relocations fact mem

let resolve fact mem default =
  match get mem (Rel_fact.relocations fact) with
  | Some a -> Some a
  | None ->
    match get mem (Rel_fact.externals fact) with
    | Some _ -> None
    | _ -> Some default

let resolve_jumps mem rels dests =
  List.map ~f:(function
      | Some addr, `Jump -> resolve rels mem addr, `Jump
      | x -> x) dests

let resolve_dests b fact mem insn =
  let dests = Brancher.resolve b mem insn in
  if has_relocations fact mem insn then
    let has_fall = List.exists ~f:(fun (_,x) -> x = `Fall) dests in
    let dests = resolve_jumps mem fact dests in
    if has_fall then dests
    else if is_return insn then
      let fall = Some (Addr.succ @@ Memory.max_addr mem), `Fall in
      fall :: resolve_jumps mem fact dests
    else dests
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
