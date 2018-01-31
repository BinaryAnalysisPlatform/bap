open Core_kernel.Std
open Bap_future.Std
open Bap.Std
open Image
open Monads.Std

include Self()

module Fact = Ogre.Make(Monad.Ident)

module Rel = struct
  open Image.Scheme
  open Fact.Syntax

  let of_seq s ~fkey ~fdata =
    Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
        Map.add m ~key:(fkey key) ~data:(fdata data))

  let addr_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let relocations =
    Fact.collect Ogre.Query.(select (from relocation))

  let external_symbols  =
    Fact.collect Ogre.Query.(
        select (from external_reference))

  let relocations =
    addr_width >>= fun width ->
    relocations >>= fun rels ->
    external_symbols >>= fun exts ->
    let to_addr = Addr.of_int64 ~width in
    Fact.return (of_seq rels ~fkey:to_addr ~fdata:to_addr,
                 of_seq exts ~fkey:to_addr ~fdata:ident)

end

let get mem data =
  let rec loop addr =
    if Addr.(addr > Memory.max_addr mem) then None
    else
      match Map.find data addr with
      | None -> loop (Addr.succ addr)
      | Some value -> Some value in
  loop (Memory.min_addr mem)

let affect_cf insn =
  Insn.(may affect_control_flow (of_basic insn))

let is_return insn = Insn.(is return (of_basic insn))

let contains_relocations (rels,exts) mem =
  Option.is_some (get mem rels) ||
  Option.is_some (get mem exts)

let has_relocations rels mem insn =
  affect_cf insn && contains_relocations rels mem

let resolve (rels,exts) mem default =
  match get mem rels with
  | Some a -> Some a
  | None ->
    match get mem exts with
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
  match Fact.eval Rel.relocations spec with
  | Ok rels ->
    Ok (Brancher.create (resolve_dests b rels))
  | Error er ->
    error "%a" Error.pp er;
    Ok b

let init () =
  let open Project.Info in
  Stream.Variadic.(apply (args arch $ spec) ~f:create) |>
  Brancher.Factory.register "relocatable"
