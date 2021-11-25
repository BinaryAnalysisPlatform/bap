open Core_kernel
open Bap_core_theory
open Bap.Std
open Arm_types

val lift :
  ?encoding:Theory.language ->
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
