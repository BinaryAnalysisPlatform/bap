open Bap_core_theory
open Thumb_core

module Make(CT : Theory.Core) : sig
  open Theory

  (** [adds rd, rn, #x] aka add(1)  *)
  val addi3 : r32 reg -> r32 reg -> int -> unit eff

  (** [adds rd, #x] aka add(2) *)
  val addi8 : r32 reg -> int -> unit eff

  (** [adds rd,rn,rm] aka add(3) and add(4) *)
  val addrr : r32 reg -> r32 reg -> r32 reg -> unit eff

  (** [add rd, sp, #x] aka add(6) *)
  val addrspi : r32 reg -> int -> unit eff

  (** [add sp, #x] aka add(7)  *)
  val addspi : int -> unit eff

  (** [subs rd, rn, #x] aka sub(1) *)
  val subi3 : r32 reg -> r32 reg -> int -> unit eff

  (** [subs rd, #x] aka sub(2)  *)
  val subi8 : r32 reg -> int -> unit eff

  (** [subs rd, rn, rm] aka sub(3) *)
  val subrr : r32 reg -> r32 reg -> r32 reg -> unit eff

  (** [subs sp, #i] aka sub(4) *)
  val subspi : int -> unit eff

  (** [mov rd, #x]  *)
  val movi8 : r32 reg -> int -> unit eff

  (** [movs rd, rn]  *)
  val movsr : r32 reg -> r32 reg -> unit eff

  (** [mov rd, rn] with [d] or [n] greater than 7.  *)
  val movr  : r32 reg -> r32 reg -> unit eff

  (** [asrs rd, rn, #i]  *)
  val asri : r32 reg -> r32 reg -> int -> unit eff

  val cmpi8 : r32 reg -> int -> unit eff
end
