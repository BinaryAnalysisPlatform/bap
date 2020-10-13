open Bap_core_theory
open Theory
open Thumb_core

type eff = unit effect KB.t

module Make(Core : Theory.Core) : sig

  (** [ldr rd, [rn, #i]] *)
  val ldri : r32 reg -> r32 reg -> int -> eff

  (** [ldr rd, [rn, rm]]  *)
  val ldrr : r32 reg -> r32 reg -> r32 reg -> eff
  (** [ldrb rd, [rn, #i]]  *)

  (** [ldrb rd, [rn, #i]]  *)
  val ldrbi : r32 reg -> r32 reg -> int -> eff

  (** [ldrb rd, [rn, rm]]  *)
  val ldrbr : r32 reg -> r32 reg -> r32 reg -> eff

  (** [ldrsb rd, [rn, rm]]  *)
  val ldrsb : r32 reg -> r32 reg -> r32 reg -> eff

  (** [ldrh rd, [rn, #i]]  *)
  val ldrhi : r32 reg -> r32 reg -> int -> eff

  (** [ldrh rd, [rn, rm]]  *)
  val ldrhr : r32 reg -> r32 reg -> r32 reg -> eff

  (** [ldrsh rd, [rn, rm]] *)
  val ldrsh : r32 reg -> r32 reg -> r32 reg -> eff

  (** [ldr rd <label>]  *)
  val ldrpci : r32 reg -> Bitvec.t -> int -> eff

  (** [ldm b!, {rm,...,rn}]  *)
  val ldm : r32 reg -> r32 reg list -> eff

  (** [str rd, [rm, #i]] *)
  val stri : r32 reg -> r32 reg -> int -> eff

  (** [str rd, [rm, rn]]  *)
  val strr : r32 reg -> r32 reg -> r32 reg -> eff

  (** [strh rd, [rm, #i]]  *)
  val strhi : r32 reg -> r32 reg -> int -> eff

  (** [strh rd, [rm, rn]]  *)
  val strhr : r32 reg -> r32 reg -> r32 reg -> eff

  (** [strb rd, [rm, #i]]  *)
  val strbi : r32 reg -> r32 reg -> int -> eff

  (** [strb rd, [rm, rn]]  *)
  val strbr : r32 reg -> r32 reg -> r32 reg -> eff

  (** [stm b!, {rm,...,rn}]  *)
  val stm : r32 reg -> r32 reg list -> eff

  (** [pop {rm,...,rn}]  *)
  val pop : r32 reg list -> eff

  (** [pop {rm,...,rn,pc}]  *)
  val popret : r32 reg list -> eff

  (** [push {rm,...,rn}]  *)
  val push : r32 reg list -> eff
end
