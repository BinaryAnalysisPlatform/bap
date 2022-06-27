open Bap_core_theory
open Thumb_core
open Thumb_opcodes

module Make(_ : Theory.Core) : sig
  open Theory
  val sx : 'a Bitv.t Value.sort -> r32 reg -> _ reg -> cond -> unit eff
  val ux : 'a Bitv.t Value.sort -> r32 reg -> _ reg -> cond -> unit eff
end
