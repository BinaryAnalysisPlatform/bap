open Bap_core_theory
open Theory
open Thumb_core
open Thumb_opcodes

type eff = unit effect KB.t
module Make(CT : Theory.Core) : sig

  (** [b <label>]  *)
  val b : Bitvec.t -> int -> eff

  (** [bcc <label>]  *)
  val bcc : Bitvec.t -> cond -> int -> eff

  (** [bl <label>] *)
  val bli : Bitvec.t -> int -> eff

  (** [blx <label>]  *)
  val blxi : Bitvec.t -> int -> eff

  (** [blx rm]  *)
  val blxr : Bitvec.t -> r32 reg -> eff

  (** [bx rm]  *)
  val bxr : r32 reg -> eff

  (** [bx <label>]  *)
  val bxi : Bitvec.t -> int -> eff

  (** [bl rm] or [blx rm]  *)
  val blr : Bitvec.t -> r32 reg -> eff
end
