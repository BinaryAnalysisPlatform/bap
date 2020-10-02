open Bap_core_theory


type r64 and r32 and r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

val r64 : r64 bitv
val r32 : r32 bitv
val r16 : r16 bitv
val r8  : r8 bitv

val mem : (r64, r8) Theory.Mem.t Theory.var
val gpr : r64 Theory.Bitv.t Theory.var list
val fpr : r64 Theory.Bitv.t Theory.var list

val parent : Theory.Target.t

val z9 : Theory.Target.t

val llvm_encoding : Theory.Language.t
