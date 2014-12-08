open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_arm_types

val lift :
  ?dest:op ->
  op ->
  ?src2:op ->
  data_oper ->
  ?sreg:op ->
  ?simm:op ->
  Word.t ->
  wflag:op ->
  op ->
  stmt list
