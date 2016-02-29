open Core_kernel.Std
open Bap.Std
open Arm_types

val lift_mull :
  lodest:op ->
  hidest:op ->
  src1:op -> src2:op -> sign -> ?addend:'a -> wflag:op -> op -> stmt list

val lift_smul :
  dest:op ->
  ?hidest:op ->
  src1:op ->
  src2:op ->
  ?accum:op -> ?hiaccum:op -> ?q:bool -> smul_size -> op -> stmt list
