open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_arm_types

val lift_r :
  dst1:var ->
  ?dst2:var ->
  base:var ->
  offset:exp ->
  mode_r ->
  sign ->
  size ->
  operation -> stmt list

val lift_m : var list -> var -> mode_m -> update_m -> operation -> stmt list
