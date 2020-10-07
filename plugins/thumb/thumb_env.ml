open Bap_core_theory
open Base
open KB.Syntax

module Env = struct
  type value
  type byte
  type reg_single
  type half_byte
  type half_word
  type bit_val

  let bit : Theory.Bool.t Theory.Value.sort =
    Theory.Bool.t

  let bit_val : bit_val Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 1

  (** grps are defined by 3-bit indices *)
  let reg_single : reg_single Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 3

  let half_word : half_word Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 16

  let value : value Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 32

  let half_byte : half_byte Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 4

  let byte : byte Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 8

  let heap_type = Theory.Mem.define value byte

  let memory = Theory.Var.define heap_type "mem"

  (** define grps *)
  let r0 = Theory.Var.define value "R0"
  let r1 = Theory.Var.define value "R1"
  let r2 = Theory.Var.define value "R2"
  let r3 = Theory.Var.define value "R3"
  let r4 = Theory.Var.define value "R4"
  let r5 = Theory.Var.define value "R5"
  let r6 = Theory.Var.define value "R6"
  let r7 = Theory.Var.define value "R7"

  (** grps which are not commonly accessible *)
  let r8 = Theory.Var.define value "R8"
  let r9 = Theory.Var.define value "R9"
  let r10 = Theory.Var.define value "R10"
  let r11 = Theory.Var.define value "R11"
  let r12 = Theory.Var.define value "R12"

  let lr = Theory.Var.define value "LR"
  (* let pc = Theory.Var.define value "PC" *)
  let sp = Theory.Var.define value "SP"
  (* The following are temporarily not used *)
  let spsr = Theory.Var.define value "SPSR"
  let cpsr = Theory.Var.define value "CPSR"
  (* The following are individual flag *)
  let nf = Theory.Var.define bit "NF"
  let zf = Theory.Var.define bit "ZF"
  let cf = Theory.Var.define bit "CF"
  let vf = Theory.Var.define bit "VF"
  let qf = Theory.Var.define bit "QF"
  let ge = Theory.Var.define half_byte "GE"

  module Defs = Thumb_defs

  type reg_type = Defs.reg
  type operand = Defs.op

  (* to elimiate ambiguity *)
  let load_reg_wide (op : Defs.reg) = let open Thumb_defs in
    match op with
    | `R0 -> r0
    | `R1 -> r1
    | `R2 -> r2
    | `R3 -> r3
    | `R4 -> r4
    | `R5 -> r5
    | `R6 -> r6
    | `R7 -> r7
    | `R8 -> r8
    | `R9 -> r9
    | `R10 -> r10
    | `R11 -> r11
    | `R12 -> r12
    | `LR -> lr
    | `SP -> sp
    | `PC (* pc should never normally occur *)
    | _ -> failwith "unexpected or unknown register"

  let load_reg (op : Defs.reg) = let open Thumb_defs in
    match op with
    | `R0 -> r0
    | `R1 -> r1
    | `R2 -> r2
    | `R3 -> r3
    | `R4 -> r4
    | `R5 -> r5
    | `R6 -> r6
    | `R7 -> r7
    | `LR -> lr
    | `SP -> sp
    | `PC (* pc should never normally occur *)
    | _ -> failwith "unexpected or unknown register"

end
