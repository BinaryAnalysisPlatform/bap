open Bap_core_theory
open Theory

module Env = Thumb_env.Env

type reg = Env.value Bitv.t var
type eff = unit effect KB.t
type cnd = Thumb_defs.cond

module Make(Core : Theory.Core) : sig

  (** [b <label>]  *)
  val b : Bitvec.t -> int -> eff

  (** [bcc <label>]  *)
  val bcc : Bitvec.t -> cnd -> int -> eff

  (** [bl #off] or [blx #off] *)
  val bli : Bitvec.t -> int -> eff

  (** [bl rm] or [blx rm]  *)
  val blr : Bitvec.t -> reg -> eff
end
