open Core_kernel.Std
open Bap.Std
open Arm_types

val lift :
  ?dest:op ->
  op ->
  ?src2:op ->
  data_oper ->
  ?sreg:op ->
  ?simm:op ->
  word ->
  wflag:op ->
  op ->
  stmt list
