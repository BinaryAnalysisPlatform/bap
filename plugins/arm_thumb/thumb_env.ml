open Bap_core_theory
open Base
open KB.Syntax

module Env = struct
type value
type byte
type reg_single
type half_byte
type half_word

  let bit : Theory.Bool.t Theory.Value.sort =
    Theory.Bool.t

  (** grps are defined by 3-bit indices *)
  let reg_single : reg_single Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 3

  let half_word : half_word Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 32

  let value : value Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 32

  let half_byte : half_byte Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 4

  let byte : byte Theory.Bitv.t Theory.Value.sort =
    Theory.Bitv.define 8

  let heap_type = Theory.Mem.define value byte

  let memory = Theory.Var.define heap_type "mem"

  (** define grps *)
  let r0 = Theory.Var.define value "r0"
  let r1 = Theory.Var.define value "r1"
  let r2 = Theory.Var.define value "r2"
  let r3 = Theory.Var.define value "r3"
  let r4 = Theory.Var.define value "r4"
  let r5 = Theory.Var.define value "r5"
  let r6 = Theory.Var.define value "r6"
  let r7 = Theory.Var.define value "r7"

  (** grps which are not commonly accessible *)
  let r8 = Theory.Var.define value "r8"
  let r9 = Theory.Var.define value "r9"
  let r10 = Theory.Var.define value "r10"
  let r11 = Theory.Var.define value "r11"
  let r12 = Theory.Var.define value "r12"

  let lr = Theory.Var.define value "lr"
  let pc = Theory.Var.define value "pc"
  let sp = Theory.Var.define value "sp"
(* The following are temporarily not used *)
  let spsr = Theory.Var.define value "spsr"
  let cpsr = Theory.Var.define value "cpsr"
(* The following are individual flag *)
  let nf = Theory.Var.define bit "nf"
  let zf = Theory.Var.define bit "zf"
  let cf = Theory.Var.define bit "cf"
  let vf = Theory.Var.define bit "vf"
  let qf = Theory.Var.define bit "qf"
  let ge = Theory.Var.define half_byte "ge"
(* psuedo register storing temporary computations *)
  let tmp = Theory.Var.define value "tmp"

exception Unbound_Reg
module Defs = Thumb_defs

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
    | `PC -> pc
    | _ -> raise Unbound_Reg

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
    | `PC -> pc
    | _ -> raise Unbound_Reg

end