(* This module is left temporary for backward compatibility,
   we will remove it later.
*)
open Core_kernel
open Bap_core_theory
open KB.Syntax

open Thumb_core

module Env = struct
  type value = r32
  type byte = r8
  type half_word = r16
  type bit = r1

  let bit = s1
  let half_word = s16
  let value = s32
  let byte = s8
  let memory = mem
  let r0 = r0
  let r1 = r1
  let r2 = r2
  let r3 = r3
  let r4 = r4
  let r5 = r5
  let r6 = r6
  let r7 = r7
  let r8 = r8
  let r9 = r9
  let r10 = r10
  let r11 = r11
  let r12 = r12
  let lr = lr
  let sp = sp
  let nf = nf
  let zf = zf
  let cf = cf
  let vf = vf
  let qf = qf
  let tf = tf

  module Defs = Thumb_defs

  type reg_type = Defs.reg
  type operand = Defs.op

  (* to elimiate ambiguity *)
  let load_reg_wide (op : Defs.reg) = match op with
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
    | reg ->
      failwithf "load_wide: unexpected or unknown register: %s"
        (Sexp.to_string (Defs.sexp_of_reg reg))
        ()

  let load_reg (op : Defs.reg) = match op with
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
    | reg ->
      failwithf "load_reg: unexpected or unknown register: %s"
        (Sexp.to_string (Defs.sexp_of_reg reg))
        ()
end
