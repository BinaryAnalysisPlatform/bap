open Bap.Std
open Regular.Std

type t = Arm_types.insn with bin_io, compare, sexp
val create : insn -> t option
val of_basic : ('a,'b) Disasm_expert.Basic.insn -> t option

include Regular with type t := t
