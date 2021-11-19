open Bap_core_theory
open Thumb_core
open Thumb_opcodes

module Make(CT : Theory.Core) : sig
  open Theory
  val sx : r32 reg -> _ reg -> cond -> unit eff
  val ux : r32 reg -> _ reg -> cond -> unit eff
end
