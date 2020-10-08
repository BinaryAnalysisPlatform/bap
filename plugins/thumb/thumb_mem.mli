open Bap_core_theory
open Theory

module Env = Thumb_env.Env

type reg = Env.value Bitv.t var
type eff = unit effect KB.t

module Make(Core : Theory.Core) : sig

  (** [ldr rd, [rn, #i]] *)
  val ldri : reg -> reg -> int -> eff

  (** [ldr rd, [rn, rm]]  *)
  val ldrr : reg -> reg -> reg -> eff
  (** [ldrb rd, [rn, #i]]  *)

  (** [ldrb rd, [rn, #i]]  *)
  val ldrbi : reg -> reg -> int -> eff

  (** [ldrb rd, [rn, rm]]  *)
  val ldrbr : reg -> reg -> reg -> eff

  (** [ldrsb rd, [rn, rm]]  *)
  val ldrsb : reg -> reg -> reg -> eff

  (** [ldrh rd, [rn, #i]]  *)
  val ldrhi : reg -> reg -> int -> eff

  (** [ldrh rd, [rn, rm]]  *)
  val ldrhr : reg -> reg -> reg -> eff

  (** [ldrsh rd, [rn, rm]] *)
  val ldrsh : reg -> reg -> reg -> eff

  (** [ldr rd <label>]  *)
  val ldrpci : reg -> Bitvec.t -> int -> eff

  (** [ldm b!, {rm,...,rn}]  *)
  val ldm : reg -> reg list -> eff

  (** [str rd, [rm, #i]] *)
  val stri : reg -> reg -> int -> eff

  (** [str rd, [rm, rn]]  *)
  val strr : reg -> reg -> reg -> eff

  (** [strh rd, [rm, #i]]  *)
  val strhi : reg -> reg -> int -> eff

  (** [strh rd, [rm, rn]]  *)
  val strhr : reg -> reg -> reg -> eff

  (** [strb rd, [rm, #i]]  *)
  val strbi : reg -> reg -> int -> eff

  (** [strb rd, [rm, rn]]  *)
  val strbr : reg -> reg -> reg -> eff

  (** [stm b!, {rm,...,rn}]  *)
  val stm : reg -> reg list -> eff

  (** [pop {rm,...,rn}]  *)
  val pop : reg list -> eff

  (** [pop {rm,...,rn,pc}]  *)
  val popret : reg list -> eff

  (** [push {rm,...,rn}]  *)
  val push : reg list -> eff
end
