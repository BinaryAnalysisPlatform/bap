open Bap_core_theory
open Base
open KB.Syntax

module Env = Arm_env.Env

(** ARM VFP support *)
module Env_fp = struct
  type base
  type f32_exps
  type f32_sig
  type f32_bitv
  type f64_exps
  type f64_sig
  type f64_bitv

  let f32 : ((base, f32_exps, f32_sig) Theory.IEEE754.t, f32_bitv) Theory.format Theory.Float.t Theory.Value.sort =
    Theory.IEEE754.Sort.define Theory.IEEE754.binary32

  let f32_exps = Theory.IEEE754.Sort.exps f32

  let f32_sig = Theory.IEEE754.Sort.sigs f32

  let f32_bitv = Theory.IEEE754.Sort.bits f32

  type f64_format = ((base, f64_exps, f64_sig) Theory.IEEE754.t, f32_bitv) Theory.format

  let f64 : f64_format Theory.Float.t Theory.Value.sort =
    Theory.IEEE754.Sort.define Theory.IEEE754.binary64

  let f64_exps = Theory.IEEE754.Sort.exps f64

  let f64_sig = Theory.IEEE754.Sort.sigs f64

  let f64_bitv = Theory.IEEE754.Sort.bits f64

  let d0 = Theory.Var.define f64 "d0"
  let d1 = Theory.Var.define f64 "d1"
  let d2 = Theory.Var.define f64 "d2"
  let d3 = Theory.Var.define f64 "d3"
  let d4 = Theory.Var.define f64 "d4"
  let d5 = Theory.Var.define f64 "d5"
  let d6 = Theory.Var.define f64 "d6"
  let d7 = Theory.Var.define f64 "d7"
  let d8 = Theory.Var.define f64 "d8"
  let d9 = Theory.Var.define f64 "d9"
  let d10 = Theory.Var.define f64 "d10"
  let d11 = Theory.Var.define f64 "d11"
  let d12 = Theory.Var.define f64 "d12"
  let d13 = Theory.Var.define f64 "d13"
  let d14 = Theory.Var.define f64 "d14"
  let d15 = Theory.Var.define f64 "d15"

  let tmp = Theory.Var.define f32 "ftmp"
  let tmp64 = Theory.Var.define f64 "ftmp64"

  let fpscr = Theory.Var.define Env.value "fpscr"
  let nf = Theory.Var.define Env.bit "fnf"
  let zf = Theory.Var.define Env.bit "fzf"
  let cf = Theory.Var.define Env.bit "fcf"
  let vf = Theory.Var.define Env.bit "fvf"

  (* consider the fact that single precision is overlapping *)
  type access =  [
    | `Whole
    | `Hi
    | `Lo
  ]

  exception Unbound_Reg
  module Defs = Arm_defs

  type reg_type = Defs.reg
  type operand = Defs.op

  let load_dreg (op : reg_type) = let open Defs in
    match op with
    | `D0 -> (d0, `Whole)
    | `D1 -> (d1, `Whole)
    | `D2 -> (d2, `Whole)
    | `D3 -> (d3, `Whole)
    | `D4 -> (d4, `Whole)
    | `D5 -> (d5, `Whole)
    | `D6 -> (d6, `Whole)
    | `D7 -> (d7, `Whole)
    | `D8 -> (d8, `Whole)
    | `D9 -> (d9, `Whole)
    | `D10 -> (d10, `Whole)
    | `D11 -> (d11, `Whole)
    | `D12 -> (d12, `Whole)
    | `D13 -> (d13, `Whole)
    | `D14 -> (d14, `Whole)
    | `D15 -> (d15, `Whole)
    | _ -> raise Unbound_Reg

  let load_sreg (op : reg_type) = let open Defs in
    match op with
    | `S0 -> (d0, `Lo)
    | `S1 -> (d0, `Hi)
    | `S2 -> (d1, `Lo)
    | `S3 -> (d1, `Hi)
    | `S4 -> (d2, `Lo)
    | `S5 -> (d2, `Hi)
    | `S6 -> (d3, `Lo)
    | `S7 -> (d3, `Hi)
    | `S8 -> (d4, `Lo)
    | `S9 -> (d4, `Hi)
    | `S10 -> (d5, `Lo)
    | `S11 -> (d5, `Hi)
    | `S12 -> (d6, `Lo)
    | `S13 -> (d6, `Hi)
    | `S14 -> (d7, `Lo)
    | `S15 -> (d7, `Hi)
    | `S16 -> (d8, `Lo)
    | `S17 -> (d8, `Hi)
    | `S18 -> (d9, `Lo)
    | `S19 -> (d9, `Hi)
    | `S20 -> (d10, `Lo)
    | `S21 -> (d10, `Hi)
    | `S22 -> (d11, `Lo)
    | `S23 -> (d11, `Hi)
    | `S24 -> (d12, `Lo)
    | `S25 -> (d12, `Hi)
    | `S26 -> (d13, `Lo)
    | `S27 -> (d13, `Hi)
    | `S28 -> (d14, `Lo)
    | `S29 -> (d14, `Hi)
    | `S30 -> (d15, `Lo)
    | `S31 -> (d15, `Hi)
    | _ -> raise Unbound_Reg

end