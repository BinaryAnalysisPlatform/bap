open Bap_core_theory
open Thumb_core

module Make(CT : Theory.Core) : sig
  open Theory
  val sx : r32 reg -> _ reg -> unit eff
  val ux : r32 reg -> _ reg -> unit eff
end
