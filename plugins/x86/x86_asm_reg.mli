open Core_kernel.Std
open Bap.Std

include module type of X86_asm_reg_types

val width : [gpr | ip] -> size
val bitwidth : [gpr | ip] -> int
val decode : reg -> t option
