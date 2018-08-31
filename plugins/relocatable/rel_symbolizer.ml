open Core_kernel
open Bap.Std
open Bap_future.Std
open Monads.Std

include Self ()

module Fact = Ogre.Make(Monad.Ident)

module Rel = struct
  open Image.Scheme
  open Fact.Syntax

  let of_aseq s =
    Seq.fold s ~init:Addr.Map.empty ~f:(fun m (key,data) ->
        Map.add m ~key ~data)

  let arch_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let external_symbols  =
    arch_width >>= fun width ->
    Fact.collect Ogre.Query.(
        select (from external_reference)) >>= fun s ->
    Fact.return
      (of_aseq @@ Seq.map s ~f:(fun (addr, data) ->
           Addr.of_int64 ~width addr, data))
end

let find start len exts =
  Seq.find_map ~f:(Map.find exts) @@ Seq.init len ~f:(Addr.nsucc start)

let create cfg exts =
  let insns = Disasm.create cfg |> Disasm.insns in
  Seq.fold insns ~init:Addr.Map.empty
    ~f:(fun calls (m,_) ->
        let min = Memory.min_addr m in
        let len = Memory.length m in
        match find min len exts with
        | None -> calls
        | Some name -> Map.add calls min name)

let init () =
  let open Project.Info in
  Stream.Variadic.(apply (args cfg $ spec) ~f:(fun cfg spec ->
      let name =
        match Fact.eval Rel.external_symbols spec with
        | Ok exts -> Map.find (create cfg exts)
        | _ -> fun _ -> None in
      Ok (Symbolizer.create name))) |>
  Symbolizer.Factory.register "relocatable"

let () =
  init ();
  Config.manpage [
    `S "SYNOPSIS";
    `S "DESCRIPTION";
    `P "Provides a symbolizer for external symbols in relocatable files.";
    `P "Relocatable symbolizer provides correct addresses of a calling
        side of external functions. Usually, relocations contains
        address where fixup should be done, and this address
        doesn't match with an address of any instruction, because it
        points somewhere to the middle of the instruction bytes, after
        prefixes, opcodes and etc. Relocatable symbolizer uses OGRE
        to query relocations and disassembled instructions to find
        an instruction address of external function calls.";
    `S "SEE ALSO";
    `P "$(b,bap-plugin-llvm)(1) code";
  ];
  Config.when_ready (fun _ -> ())
