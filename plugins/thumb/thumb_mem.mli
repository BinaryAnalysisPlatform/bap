open Bap_core_theory
open Theory

module Env = Thumb_env.Env

type reg = Env.value Bitv.t var
type eff = unit effect KB.t

module Make(Core : Theory.Core) : sig

  (** [ldr rd, [rn, #i]] *)
  val ldri : reg -> reg -> int64 -> eff

  (** [ldr rd, [rn, rm]]  *)
  val ldrr : reg -> reg -> reg -> eff
  (** [ldrb rd, [rn, #i]]  *)

  (** [ldrb rd, [rn, #i]]  *)
  val ldrbi : reg -> reg -> int64 -> eff

  (** [ldrb rd, [rn, rm]]  *)
  val ldrbr : reg -> reg -> reg -> eff

  (** [ldrsb rd, [rn, rm]]  *)
  val ldrsb : reg -> reg -> reg -> eff

  (** [ldrh rd, [rn, #i]]  *)
  val ldrhi : reg -> reg -> int64 -> eff

  (** [ldrh rd, [rn, rm]]  *)
  val ldrhr : reg -> reg -> reg -> eff

  (** [ldrsh rd, [rn, rm]] *)
  val ldrsh : reg -> reg -> reg -> eff

  (** [ldr rd <label>]  *)
  val ldrpci : reg -> Bitvec.t -> int64 -> eff

  (** [ldr rd [sp, #i]]  *)
  val ldrspi : reg -> int64 -> eff

  (** [ldm b! {rm,...,rn}]  *)
  val ldmu : reg -> reg list -> eff

  (** [ldm b, {rm,...,rn}]  *)
  val ldm : reg -> reg list -> eff

  (** [str rd, [rm, #i]] *)
  val stri : reg -> reg -> int64 -> eff

  (** [str rd, [rm, rn]]  *)
  val strr : reg -> reg -> reg -> eff

  (** [strh rd, [rm, #i]]  *)
  val strhi : reg -> reg -> int64 -> eff

  (** [strh rd, [rm, rn]]  *)
  val strhr : reg -> reg -> reg -> eff

  (** [strb rd, [rm, #i]]  *)
  val strbi : reg -> reg -> int64 -> eff

  (** [strb rd, [rm, rn]]  *)
  val strbr : reg -> reg -> reg -> eff

  (** [str rd, [sp, #i]]  *)
  val strsp : reg -> int64 -> eff

  (** [stm b!, {rm,...,rn}]  *)
  val stm : reg -> reg list -> eff
end
