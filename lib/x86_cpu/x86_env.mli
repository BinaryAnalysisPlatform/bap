open Bap.Std
open X86_types

(** {2 condition flag bits} *)


(** carry flag  *)
val cf : var

(** parity flag  *)
val pf : var

(** adjust flag  *)
val af : var

(** zero flag  *)
val zf : var

(** sign flag  *)
val sf : var

(** overflow flag  *)
val oF : var

(** direction flag  *)
val df : var

(** code segment  *)
val cs : var

(** data segment  *)
val ds : var

(** extra data segment #1 *)
val es : var

(** extra data segment #2 *)
val fs : var

(** extra data segment #3  *)
val gs : var

(** stack segment  *)
val ss : var

(** fpu control register  *)
val fpu_ctrl : var


(** mx status control register  *)
val mxcsr    : var



val o_rax : operand
val o_rcx : operand
val o_rdx : operand
val o_rbx : operand
val o_rsp : operand
val o_rbp : operand
val o_fs  : operand
val o_gs  : operand

val pref_lock : int
val repnz     : int
val repz      : int
val hint_bnt  : int
val hint_bt : int
val pref_cs : int
val pref_ss : int
val pref_ds : int
val pref_es : int
val pref_fs : int
val pref_gs : int
val pref_opsize : int
val pref_addrsize : int

(** Prefixes that we can usually handle automatically *)
val standard_prefs : int list


(** CPU BIL variables.

    For simplicity we're using the same names for registers in
    32 and 64 mode. For example, the A register, has a name [rax] on
    both 32-bit and 64-bit processors. However, on the former it is
    32-bit (contrary to the name), and on the latter it is 64-bit.

  *)
module type ModeVars = sig

  (** base pointer *)
  val rbp : var

  (** stack pointer  *)
  val rsp : var

  (** source index  *)
  val rsi : var

  (** destination index  *)
  val rdi : var

  (** instruction pointer  *)
  val rip : var

  (** accumulator register *)
  val rax : var

  (** base register *)
  val rbx : var

  (** counter register *)
  val rcx : var

  (** data register *)
  val rdx : var


  (** RFLAGS register  *)
  val rflags  : var


  (** Global Descriptor Table  *)
  val gdt : var


  (** Local Descriptor Table  *)
  val ldt : var

  (** segment registers let bases *)
  val fs_base : var
  val gs_base : var

  val seg_ss : var option
  val seg_es : var option
  val seg_cs : var option
  val seg_ds : var option
  val seg_fs : var option
  val seg_gs : var option


  (** memory  *)
  val mem : var
  (* r8 -> r15 *)

  (** r8-r15 registers.
      Due to a legacy issues r.(0) -> r8, r.(1) -> r8, ... *)
  val r : var array

  [@@deprecated "[since 2018-01] user `r` instead"]
  (** Legacy version of the `r` array, use `r` instead. *)
  val nums : var array

  (** array of YMM registers  *)
  val ymms: var array

end


(** 32-bit mode registers  *)
module R32 : ModeVars

(** 64-bit mode registers  *)
module R64 : ModeVars


(** [vars_of_mode mode] creates registers for a [mode]  *)
val vars_of_mode : mode -> (module ModeVars)
