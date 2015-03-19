open Core_kernel.Std
open Bap_types.Std
open Bap_disasm_arm_types

val nil : var
val spsr : var
val cpsr : var
val nf : var
val zf : var
val cf : var
val vf : var
val qf : var
val ge : var array
val itstate : var
val lr : var
val pc : var
val sp : var
val r0 : var
val r1 : var
val r2 : var
val r3 : var
val r4 : var
val r5 : var
val r6 : var
val r7 : var
val r8 : var
val r9 : var
val r10 : var
val r11 : var
val r12 : var
val r13 : var
val r14 : var
val r15 : var

val of_reg : reg -> var

val new_var : string -> var

val new_tmp : string -> var

val mem : var
