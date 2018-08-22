open Core_kernel.Std
open Bap_future.Std
open Bap.Std
open Image
open Monads.Std
open Regular.Std
open Bap_service

include Self()

module Fact = Ogre.Make(Monad.Ident)

let of_aseq width x =
  Seq.fold x ~init:Addr.Map.empty ~f:(fun m (key,data) ->
      let key = Addr.of_int64 ~width key in
      Map.add m ~key ~data)

module Rel = struct
  open Image.Scheme
  open Fact.Syntax

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
    external_symbols >>= fun ext ->
    Fact.return (of_aseq width rels, of_aseq width ext)

end

let width_of_mem m = Word.bitwidth (Memory.min_addr m)

let get mem data =
  let rec find addr =
    if Addr.(addr > Memory.max_addr mem) then None
    else
      match Map.find data addr with
      | None -> find (Addr.succ addr)
      | Some value -> Some value in
  find (Memory.min_addr mem)

let relocate rels mem =
  let width = width_of_mem mem in
  Option.map (get mem rels) ~f:(Addr.of_int64 ~width)

let nullify_call exts mem =
  let width = width_of_mem mem in
  Option.map (get mem exts) ~f:(fun _ -> Addr.zero width)

let affect_cf insn =
  Insn.(may affect_control_flow (of_basic insn))

let contains_relocations (rels,exts) mem =
  Option.is_some (get mem rels) || Option.is_some (get mem exts)

let has_relocations rels mem insn =
  affect_cf insn && contains_relocations rels mem

let resolve (rels, exts) mem default =
  match relocate rels mem with
  | Some a -> a
  | None ->
    Option.value_map ~default ~f:ident (nullify_call exts mem)

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
  match Fact.eval Rel.relocations spec with
  | Ok rels ->
    Ok (Brancher.create (resolve_dests b rels))
  | Error er ->
    error "%a" Error.pp er;
    Ok b

let init () =
  let open Project.Info in
  Stream.Variadic.(apply (args arch $ spec) ~f:create) |>
  Brancher.Factory.register name


let rel_brancher = Service.(begin
    provide brancher "edu.cmu.ece.bap.brancher"
      ~desc:"extracts branch destinations from the relocation table" [
      required loader;
      parameter Config.input;
    ]
  end)
