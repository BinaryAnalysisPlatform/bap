open Bap_core_theory
open Thumb_opcodes

open Theory

type r1 and r8 and r16 and r32

type 'a reg = 'a Bitv.t var

val s1  : r1 Bitv.t Value.sort
val s8  : r8 Bitv.t Value.sort
val s16 : r16 Bitv.t Value.sort
val s32 : r32 Bitv.t Value.sort

val r0 : r32 reg
val r1 : r32 reg
val r2 : r32 reg
val r3 : r32 reg
val r4 : r32 reg
val r5 : r32 reg
val r6 : r32 reg
val r7 : r32 reg
val r8 : r32 reg
val r9 : r32 reg
val r10 : r32 reg
val r11 : r32 reg
val r12 : r32 reg
val lr : r32 reg
val sp : r32 reg

val nf : r1 reg
val zf : r1 reg
val cf : r1 reg
val vf : r1 reg
val qf : r1 reg
val tf : r1 reg

val mem : (r32, r8) Mem.t var

module W1  : Bitvec.S with type 'a m = 'a
module W8  : Bitvec.S with type 'a m = 'a
module W32 : Bitvec.S with type 'a m = 'a

module Make(CT : Theory.Core) : sig
  val var : 'a var -> 'a pure
  val seq : 'a eff list -> 'a eff
  val foreach : 'a list -> ('a -> 'b eff list) -> 'b eff
  val foreachi : 'a list -> (int -> 'a -> 'b eff list) -> 'b eff
  val bitv : Bitvec.t -> r32 bitv
  val const : int -> r32 bitv

  val load : 'a Bitv.t Value.sort -> r32 bitv -> 'a bitv
  val store : r32 bitv -> 'a bitv -> data eff


  (** [holds cnd] evaluates to true iff [cnd] holds. *)
  val holds : cond -> Theory.bool

  (** [it_set v x k] assigns [x] to [v] in the scope of IT block.

      Creates a function [cond -> unit eff] that performs two
      different operations depending on whether its applied inside of
      an IT block or not. An instruction is assumed to be in the IT
      block if the passed condition is not [AL].

      If inside an IT block, the assignment is turned into a
      conditional, expression, [v := c ? x : v].

      If outside of an IT block then a fresh variable [t] is created and
      bound to [x], it is then passed to [t] and finally, [v] is set
      to [t], i.e., the resulting compuation is [t := x; k t; v := t].
  *)
  val it_set : 'a var -> 'a pure -> ('a var -> data eff list) -> cond ->
    unit eff

  (** [null] is shorcut for [Theory.Label.null]  *)
  val null : Theory.label

  (** [label] a fresh label  *)
  val label : Theory.label KB.t

  (** [data eff] labels [eff] as data effects  *)
  val data : data eff list -> unit eff

  (** [ctrl eff] labels [eff] as a control effect.  *)
  val ctrl : ctrl eff -> unit eff

  (** [goto dst] emits a resolved jump  *)
  val goto : Bitvec.t -> unit eff

  (** [branch c ys ns] if [c] holds evaluates [ys] else [ns]. *)
  val branch : cond -> data eff list -> data eff list -> unit eff

  (** [bool flag] translates a one-bit value to a boolean value  *)
  val bool : r1 bitv -> bool

  (** [bit0] is a one bit [0] value  *)
  val bit0 : r1 bitv

  (** [bit1] is a one bit [1] value  *)
  val bit1 : r1 bitv

  (** [bit x] translates a boolean value to a one-bit value  *)
  val bit : bool -> r1 bitv

  (** [nth n x] returns the [n]th bit of the value [x]  *)
  val nth : int -> _ bitv -> r1 bitv

  (** [msb x] is [nth 31 x]   *)
  val msb : r32 bitv -> r1 bitv

  (** [lsb x] is [nth 0 x]  *)
  val lsb : _ bitv -> r1 bitv

  (** [is_set x] evaluates to [CT.b1] if [x] is [1]  *)
  val is_set : r1 bitv -> bool

  (** [is_clear x] evaluates to [CT.b1] if [x] is [0]  *)
  val is_clear : r1 bitv -> bool

  (** [is_zero x] is [bit (CT.is_zero x)]  *)
  val is_zero : 'a bitv -> r1 bitv

  module Syntax : sig

    (** [v := x] sets [v] to [x]  *)
    val (:=) : 'a var -> 'a pure -> data eff

    val (+) : 'a bitv -> 'a bitv -> 'a bitv
    val (-) : 'a bitv -> 'a bitv -> 'a bitv

    (** [v += x] and [v -= x] increments or decrements [v] by [x] *)
    val (+=) : 'a reg -> 'a bitv -> data eff
    val (-=) : 'a reg -> 'a bitv -> data eff

    (** [addr <-- data] stores [data] at [addr] *)
    val (<--) : r32 bitv -> 'a bitv -> data eff

    (** [pc +> off] pc-relative offset  *)
    val (+>) : Bitvec.t -> int -> Bitvec.t

    (** [~@v] is the value of [v]  *)
    val (~@) : 'a reg -> 'a bitv


    (** [~?cnd] is [holds cnd]  *)
    val (~?) : cond -> Theory.bool

    (** [v %=? x] conditional assignment.

        Creates a function that turns [x] into a conditional
        expression. If [cond] is [`AL], then the assignment
        is unconditional. *)
    val (<-?) : 'a var -> 'a pure -> cond -> unit eff


    (** [p <--? x] conditional store.

        if [cond] is not [`AL] turns the store into a conditional
        expression, [mem := cnd ? mem[p] <- x : mem]  *)
    val (<--?) : r32 bitv -> 'a bitv -> cond -> unit eff

    val (=) : 'a bitv -> 'a bitv -> bool
    val (<>) : 'a bitv -> 'a bitv -> bool
    val (<=) : 'a bitv -> 'a bitv -> bool
    val (>=) : 'a bitv -> 'a bitv -> bool
    val (<) : 'a bitv -> 'a bitv -> bool
    val (>) : 'a bitv -> 'a bitv -> bool
    val (<=$) : 'a bitv -> 'a bitv -> bool
    val (>=$) : 'a bitv -> 'a bitv -> bool
    val (<$) : 'a bitv -> 'a bitv -> bool
    val (>=$) : 'a bitv -> 'a bitv -> bool
    val (&&) : bool -> bool -> bool
    val (||) : bool -> bool -> bool
    val not : bool -> bool


    val (land) : 'a bitv -> 'a bitv -> 'a bitv
    val (lor) : 'a bitv -> 'a bitv -> 'a bitv
    val (lxor) : 'a bitv -> 'a bitv -> 'a bitv
    val (lsl) : 'a bitv -> _ bitv -> 'a bitv
    val (lsr) : 'a bitv -> _ bitv -> 'a bitv
    val (asr) : 'a bitv -> _ bitv -> 'a bitv
    val lnot : 'a bitv -> 'a bitv
  end

end
