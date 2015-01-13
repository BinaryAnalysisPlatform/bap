open Core_kernel.Std
open Bap_types.Std

module Arm = Bap_disasm_arm

let (%:) name typ = Var.create name typ


let nil = Arm.Reg.to_string `Nil %: reg32_t

let make_register reg ty = Arm.Reg.to_string reg %: ty
let reg32 reg = make_register reg reg32_t

(* Saved Program Status Register *)
let spsr = reg32 `SPSR
let cpsr = reg32 `CPSR


(* Memory definition *)
(* let mem = new_var "mem32" (TMem (Reg 32, Reg 8)) *)

(* Arithmetic flags, individually *)
let nf = "NF" %: bool_t
let zf = "ZF" %: bool_t
let cf = "CF" %: bool_t
let vf = "VF" %: bool_t
let qf = "QF" %: bool_t
let ge = Array.init 4 ~f:(fun n -> sprintf "GE%d" n %: bool_t)


(* Thumb if-then state register *)
let itstate = Arm.Reg.to_string `ITSTATE %: reg8_t

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

let var_of_gpr : Arm.Reg.gpr -> var = function
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

let var_of_ccr : Arm.Reg.ccr -> var = function
  | `CPSR -> cpsr
  | `SPSR -> spsr
  | `ITSTATE -> itstate

let of_reg : Arm.Reg.t -> var = function
  | `Nil -> nil
  | #Arm.Reg.gpr as reg -> var_of_gpr reg
  | #Arm.Reg.ccr as reg -> var_of_ccr reg


let new_var name = Var.create name reg32_t
let new_tmp name = Var.create ~tmp:true name reg32_t
let new_mem name = Var.create name (mem32_t `r32)
