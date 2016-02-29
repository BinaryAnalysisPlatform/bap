open Core_kernel.Std
open Bap.Std
open X86_types

val parse_instr: mode -> mem -> addr -> int list * prefix * opcode * addr

val parse_prefixes: mode -> int list -> opcode -> var option * int List.t
