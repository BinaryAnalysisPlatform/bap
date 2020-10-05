open Bap_core_theory

val parent : Theory.target
val atmega328 : Theory.target
val llvm_avr16 : Theory.language

type r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

val r16 : r16 bitv
val r8  : r8 bitv

val code : (r16, r16) Theory.Mem.t Theory.var
val data : (r16, r8) Theory.Mem.t Theory.var
val gpr : r8 Theory.Bitv.t Theory.var list
val sp : r16 Theory.Bitv.t Theory.var
val flags : Theory.Bool.t Theory.var list
