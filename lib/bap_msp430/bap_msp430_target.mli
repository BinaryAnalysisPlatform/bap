open Bap_core_theory


type r16 and r8

type 'a bitv = 'a Theory.Bitv.t Theory.Value.sort

val msp430 : Theory.Target.t
val llvm16 : Theory.Language.t
