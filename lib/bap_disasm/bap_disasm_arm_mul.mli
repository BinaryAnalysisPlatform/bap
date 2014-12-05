open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_arm_types

val lift_mull :
  lodest:op ->
  hidest:op ->
  src1:op -> src2:op -> sign -> ?addend:'a -> wflag:op -> op -> stmt list

val lift_smul :
  dest:op ->
  ?hidest:Op.t ->
  src1:op ->
  src2:op ->
  ?accum:op -> ?hiaccum:op -> ?q:bool -> smul_size -> op -> stmt list
