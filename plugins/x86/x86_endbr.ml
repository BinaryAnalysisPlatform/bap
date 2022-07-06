open Core_kernel[@@warning "-D"]
open Bap_main
open Bap.Std
open X86_asm.Reg

module IA32  = X86_backend.IA32
module AMD64 = X86_backend.AMD64

type endbr = [ `ENDBR32 | `ENDBR64 ] [@@deriving bin_io, sexp, compare, enumerate]

let lift enabled _mem _insn =
  if enabled then Ok [Bil.(encode intrinsic "endbr") ]
  else Ok []

let enabled = Extension.Configuration.flag "cet-enabled"
    ~aliases:["endbr-enabled"]
    ~doc:"When enabled, translate all endbr instruction into \
          intrinsic calls. Otherwise treat them as nops"

let () =
  Extension.declare @@ fun ctxt ->
  let lift = lift @@ Extension.Configuration.get ctxt enabled in
  let name op = sexp_of_endbr op |> Sexp.to_string in
  List.iter all_of_endbr ~f:(fun op -> IA32.register (name op) lift);
  List.iter all_of_endbr ~f:(fun op -> AMD64.register (name op) lift);
  Ok ()
