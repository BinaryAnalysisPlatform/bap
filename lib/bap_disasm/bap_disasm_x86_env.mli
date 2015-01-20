open Bap_types.Std
open Bap_disasm_x86_types

(** condition flag bits *)
val cf : var
val pf : var
val af : var
val zf : var
val sf : var
val oF : var
val df : var

val cs : var
val ds : var
val es : var
val fs : var
val gs : var
val ss : var

val fpu_ctrl : var
val mxcsr    : var

val ymms: var array

val o_rax : operand
val o_rcx : operand
val o_rdx : operand
val o_rbx : operand
val o_rsp : operand
val o_rbp : operand
val o_fs  : operand
val o_gs  : operand

(** prefix names *)
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

module type ModeVars = sig
  (** registers *)
  val rbp : var
  val rsp : var
  val rsi : var
  val rdi : var
  val rip : var
  val rax : var
  val rbx : var
  val rcx : var
  val rdx : var
  val rflags  : var

  val gdt : var
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

  val mem : var
  (* r8 -> r15 *)
  val nums : var array
end

module R32 : ModeVars
module R64 : ModeVars

val vars_of_mode : mode -> (module ModeVars)
