open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_x86_types

val parse_instr: mode -> Bap_memory.t -> addr -> int list * prefix * opcode * addr

val parse_prefixes: mode -> int list -> opcode -> var option * int List.t
