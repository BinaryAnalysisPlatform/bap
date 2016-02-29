open Core_kernel.Std
open Bap.Std
open Arm_types

(** Combine Mem and Shift *)



val lift_r_exp :
  dest1:op ->
  ?dest2:op ->
  base:op ->
  offset:exp ->
  mode_r ->
  sign ->
  size ->
  operation -> stmt list


val lift_r_op :
  dest1:op ->
  ?dest2:op ->
  ?shift:op ->
  base:op ->
  offset:op ->
  mode_r ->
  sign ->
  size ->
  operation -> stmt list


val lift_m : op list -> op -> mode_m -> update_m -> operation -> stmt list

(** takes a word and converts it to an exp that is the offset for some
    memory instructions sign_mask - a bitmask that determines the bit
    in src that is the repair bit imm_mask - a bitmask that determines
    which bits in src are the immediate type - whether a set mask
    indicates a positive or negative immediate.  **)
val repair_imm : word -> sign_mask:int -> imm_mask:int -> repair -> exp

(** takes a word and a register and converts it to an exp that is the
    offset for some memory instructions sign_mask - a bitmask that
    determines the bit in src that is the negative bit rtype - whether
    a set mask indicates a positive or negative operand.  *)

val repair_reg : exp -> word -> sign_mask:int -> repair -> exp


(** Decides whether to use the register or immediate as the offset
    value Also performs conversion to remove the negative bit and the *)


(** Decides whether to use the register or immediate as the offset
    value Also performs conversion to remove the negative bit and the *)

val mem_offset_reg_or_imm_neg : op -> word -> exp


val mem_offset_reg_or_imm_pos : op -> word -> exp
