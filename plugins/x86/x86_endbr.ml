open Core_kernel
open Bap.Std
open X86_asm.Reg

module IA32  = X86_backend.IA32
module AMD64 = X86_backend.AMD64

type endbr = [ `ENDBR32 | `ENDBR64 ] [@@deriving bin_io, sexp, compare, enumerate]

let lift _mem _insn = Ok [ Bil.special "end-of-branch" ]

let () =
  let name op = sexp_of_endbr op |> Sexp.to_string in
  List.iter all_of_endbr ~f:(fun op -> IA32.register (name op) lift);
  List.iter all_of_endbr ~f:(fun op -> AMD64.register (name op) lift)
