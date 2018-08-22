(** This module exposes register functionality that is based on
    LLVM's disassembler. This module is only useful if you are
    disassembling X86 using an LLVM backend. *)

open Bap.Std

(** [base_var mode reg] given a [reg] operand provided by the LLVM
    disassembler, [base_var] returns the variable corresponding to
    the physical register for that [mode] if it exists. If no such
    register exists for our backend, None is returned.
*)
val base_var : X86_types.mode -> reg -> Var.t option