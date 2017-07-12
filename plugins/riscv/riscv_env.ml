open Core_kernel.Std
open Regular.Std
open Bap.Std
open Riscv_types

let (%:) name typ = Var.create name typ

let nil = Riscv_reg.to_string `Nil %: reg32_t

let make_register reg ty = Riscv_reg.to_string reg %: ty
let reg32 reg = make_register reg reg32_t

(* Arithmetic flags, individually *)
let nf = "NF" %: bool_t
let zf = "ZF" %: bool_t
let cf = "CF" %: bool_t
let vf = "VF" %: bool_t

let pc = reg32 `PC

(* 32-bit general-purpose registers *)
let r0  = reg32 `R0
let r1  = reg32 `R1
let r2  = reg32 `R2
let r3  = reg32 `R3
let r4  = reg32 `R4
let r5  = reg32 `R5
let r6  = reg32 `R6
let r7  = reg32 `R7
let r8  = reg32 `R8
let r9  = reg32 `R9
let r10 = reg32 `R10
let r11 = reg32 `R11
let r12 = reg32 `R12
let r13  = reg32 `R13
let r14  = reg32 `R14
let r15  = reg32 `R15
let r16  = reg32 `R16
let r17  = reg32 `R17
let r18  = reg32 `R18
let r19  = reg32 `R19
let r20  = reg32 `R20
let r21  = reg32 `R21
let r22  = reg32 `R22
let r23 = reg32 `R23
let r24 = reg32 `R24
let r25 = reg32 `R25
let r26 = reg32 `R26
let r27 = reg32 `R27
let r28 = reg32 `R28
let r29 = reg32 `R29
let r30 = reg32 `R30
let r31 = reg32 `R31

(* Core registers are aliased in RISC-V *)
let zero = r0
let ra = r1
let sp = r2
let gp = r3
let tp = r4
let fp = r8

let var_of_gpr : gpr_reg -> var = function
  | `R0  -> r0
  | `R1  -> r1
  | `R2  -> r2
  | `R3  -> r3
  | `R4  -> r4
  | `R5  -> r5
  | `R6  -> r6
  | `R7  -> r7
  | `R8  -> r8
  | `R9  -> r9
  | `R10 -> r10
  | `R11 -> r11
  | `R12 -> r12
  | `R13 -> r13
  | `R14 -> r14
  | `R15 -> r15
  | `R16 -> r16
  | `R17 -> r17
  | `R18 -> r18
  | `R19 -> r19
  | `R20 -> r20
  | `R21 -> r21
  | `R22 -> r22
  | `R23 -> r23
  | `R24 -> r24
  | `R25 -> r25
  | `R26 -> r26
  | `R27 -> r27
  | `R28 -> r28
  | `R29 -> r29
  | `R30 -> r30
  | `R31 -> r31
  | `PC  -> pc
(*
  | `SP  -> sp
  | `RA  -> ra
  | `GP  -> gp
  | `TP  -> tp
  | `FP  -> fp
*)

let of_reg : reg -> var = function
  | `Nil -> nil
  | #gpr_reg as reg -> var_of_gpr reg

let new_var name = Var.create name reg32_t
let mem = Var.create "mem" (mem32_t `r32)
