open Bap_core_theory

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

  (** [label] a fresh label  *)
  val label : Theory.label KB.t
  (** [data eff] labels [eff] as data effects  *)
  val data : data eff list -> unit eff

  (** [ctrl eff] labels [eff] as a control effect.  *)
  val ctrl : ctrl eff -> unit eff

  (** [goto dst] emits a resolved jump  *)
  val goto : Bitvec.t -> unit eff

  (** [with_result v f] is [seq @@ f r @ [set v := var r]],
      where [r] is a fresh scoped variable. *)
  val with_result : 'a var -> ('a var -> data eff list) -> unit eff

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

    (** [pc ++ off] pc-relative offset  *)
    val (++) : Bitvec.t -> int -> Bitvec.t

    (** [~@v] is the value of [v]  *)
    val (~@) : 'a reg -> 'a bitv

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
