open Core_kernel[@@warning "-D"]
open Bap.Std
module Bil = X86_legacy_bil
module MC = Disasm_expert.Basic


val run : Arch.x86 -> mem -> MC.full_insn -> Bil.Ast.program Or_error.t
