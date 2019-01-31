open Core_kernel
open Regular.Std
open Bap.Std
open Arm_types

let (%:) name typ = Var.create name typ


let nil = Arm_reg.to_string `Nil %: reg32_t

let make_register reg ty = Arm_reg.to_string reg %: ty
let reg32 reg = make_register reg reg32_t

(* Saved Program Status Register *)
let spsr = reg32 `SPSR
let cpsr = reg32 `CPSR

(* Arithmetic flags, individually *)
let nf = "NF" %: bool_t
let zf = "ZF" %: bool_t
let cf = "CF" %: bool_t
let vf = "VF" %: bool_t
let qf = "QF" %: bool_t
let ge = Array.init 4 ~f:(fun n -> sprintf "GE%d" n %: bool_t)


(* Thumb if-then state register *)
let itstate = Arm_reg.to_string `ITSTATE %: reg8_t

(* Core registers: link register, program counter, stack pointer *)
let lr = reg32 `LR
let pc = reg32 `PC
let sp = reg32 `SP

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

(* Core registers are aliased to r13-15 in ARM *)
let r13 = sp
let r14 = lr
let r15 = pc

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
  | `LR  -> lr
  | `PC  -> pc
  | `SP  -> sp

let var_of_ccr : ccr_reg -> var = function
  | `CPSR -> cpsr
  | `SPSR -> spsr
  | `ITSTATE -> itstate

let of_reg : reg -> var = function
  | `Nil -> nil
  | #gpr_reg as reg -> var_of_gpr reg
  | #ccr_reg as reg -> var_of_ccr reg

let new_var name = Var.create name reg32_t
let mem = Var.create "mem" (mem32_t `r8)
