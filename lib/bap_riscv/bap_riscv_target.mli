open Bap_core_theory


type r64 and r32 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

val parent  : Theory.Target.t
val riscv32 : Theory.Target.t
val riscv64 : Theory.Target.t
val llvm32 : Theory.Language.t
val llvm64 : Theory.Language.t
