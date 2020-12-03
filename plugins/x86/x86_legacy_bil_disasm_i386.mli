(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
module Bil = X86_legacy_bil
open Bil

exception Disasm_i386_exception of string

type binopf = Ast.exp -> Ast.exp -> Ast.exp (* below: opcode *)

type mode = [ `x86 | `x86_64 ]
val type_of_mode : mode -> Type.typ (* ocaml/syscall_models.ml *)

type order                      (* below: opcode *)
type direction
type operand
type jumptarget
module Pcmpstr :
sig
  type imm8cb
  type pcmpinfo
end
type offsetinfo

type prefix                     (* below: parse_instr *)

val regs_x86 : Var.t list      (* ocaml/asmir_vars.ml *)
val regs_x86_64 : Var.t list
val regs_full : Var.t list

module R64 :                    (* everywhere *)
sig
  val r8 : Var.t
  val r9 : Var.t
  val r10 : Var.t
  val r11 : Var.t
  val r12 : Var.t
  val r13 : Var.t
  val r14 : Var.t
  val r15 : Var.t
end

val gax : mode -> Var.t
val gcx : mode -> Var.t
val gdx : mode -> Var.t
val gbx : mode -> Var.t
val gsp : mode -> Var.t
val gbp : mode -> Var.t
val gsi : mode -> Var.t
val gdi : mode -> Var.t
val gip : mode -> Var.t
val gflags : mode -> Var.t
val gfs_base : mode -> Var.t
val ggs_base : mode -> Var.t
val gmem : mode -> Var.t

val st : Var.t array
val cf : Var.t
val pf : Var.t
val af : Var.t
val zf : Var.t
val sf : Var.t
val oF : Var.t
val df : Var.t

val cs : Var.t
val ds : Var.t
val es : Var.t
val fs : Var.t
val gs : Var.t
val ss : Var.t
val x87_ie : Var.t
val x87_de : Var.t
val x87_ze : Var.t
val x87_oe : Var.t
val x87_ue : Var.t
val x87_pe : Var.t
val x87_sf : Var.t
val x87_es : Var.t
val x87_c0 : Var.t
val x87_c1 : Var.t
val x87_c2 : Var.t
val x87_top : Var.t
val x87_c3 : Var.t
val x87_b : Var.t
val x87_status_word : Ast.exp

val x87_im : Var.t
val x87_dm : Var.t
val x87_zm : Var.t
val x87_om : Var.t
val x87_um : Var.t
val x87_pm : Var.t
val x87_6 : Var.t
val x87_7 : Var.t
val x87_pc : Var.t
val x87_rc : Var.t
val x87_x : Var.t
val x87_13 : Var.t
val x87_14 : Var.t
val x87_15 : Var.t
val x87_control_word : Ast.exp


val gymms : mode -> Var.t array

val mxcsr : Var.t

val load_s: mode -> Var.t option -> Type.typ -> Ast.exp -> Ast.exp

module ToIR :
sig
    (*
    val add_labels :            (* ocaml/asmir.ml *)
      ?asm:string ->
      address:Big_int_convenience.address ->
      length:int -> Ast.stmt list -> Ast.stmt list
    *)

  type convertable_bv = [ `reg_16 | `reg_32 | `reg_64 | `reg_80 ]

  (* TODO: remove these two calls and replace with the newer
     x87_bv_to_fp_pc and x87_fp_to_bv_pc. *)
  val bv_to_fp80 : convertable_bv -> Ast.exp -> Ast.exp
  val fp80_to_bv : convertable_bv -> Ast.exp -> Ast.exp

  val x87_bv_to_fp_pc : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp
  val x87_fp_to_bv_pc : ?rm:Type.roundmode_type -> Ast.exp -> Ast.exp

  val dec_x87_stack : Ast.stmt
  val set_x87_stack : index:int -> Ast.exp -> Ast.stmt list
  val get_x87_stack : index:int -> Ast.exp
  val get_x87_tag   : index:int -> Ast.exp
  val pop_x87_stack : Ast.stmt list
  val push_x87_stack : Ast.exp -> Ast.stmt list

  val store_s : mode -> Var.t option -> Type.typ -> Ast.exp -> Ast.exp -> Ast.stmt

  val move : Var.t -> Ast.exp -> Ast.stmt

end
