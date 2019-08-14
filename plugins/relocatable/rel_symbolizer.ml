open Bap_knowledge
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
        Map.set m ~key ~data)

  let arch_width =
    Fact.require arch >>= fun a ->
    match Arch.of_string a with
    | Some a -> Fact.return (Arch.addr_size a |> Size.in_bits)
    | None -> Fact.failf "unknown/unsupported architecture" ()

  let external_symbols  =
    arch_width >>= fun width ->
    Fact.collect
      Ogre.Query.(select (from external_reference)) >>= fun s ->
    Fact.return
      (of_aseq @@ Seq.map s ~f:(fun (addr, data) ->
           Addr.of_int64 ~width addr, data))
end

let agent = Knowledge.Agent.register
    ~package:"bap.std" "relocation-symbolizer"

let init () =
  Stream.observe Project.Info.spec @@ fun spec ->
  let name = match Fact.eval Rel.external_symbols spec with
    | Ok exts -> Map.find exts
    | _ -> fun _ -> None in
  Symbolizer.provide agent (Symbolizer.create name)

let () =
  Config.manpage [
    `S "DESCRIPTION";
    `P "Extracts symbol information from the program relocations.";

    `P "The relocation symbolizer leverages the relocation information stored in files
        to extract symbol names. Since a relocation references an external symbol which
        doesn't have an address we use an address of a callsite.";
    `S "SEE ALSO";
    `P "$(b,bap-plugin-llvm)(1) code";
  ];
  Config.when_ready (fun _ -> init ())
