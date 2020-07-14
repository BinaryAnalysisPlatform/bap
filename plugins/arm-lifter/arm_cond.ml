open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Arm_env.Env
module Defs = Arm_defs

type cond_resolved = [
  | `Var of Theory.bool
  | `Const of bool
]

module Cond(Core : Theory.Core) = struct
  open Core
  module DSL = Arm_dsl.Make(Core)
  let cond_of_word op = match Word.to_int op with
    | Ok 0 ->  `EQ
    | Ok 1 ->  `NE
    | Ok 2 ->  `CS
    | Ok 3 ->  `CC
    | Ok 4 ->  `MI
    | Ok 5 ->  `PL
    | Ok 6 ->  `VS
    | Ok 7 ->  `VC
    | Ok 8 ->  `HI
    | Ok 9 ->  `LS
    | Ok 10 -> `GE
    | Ok 11 -> `LT
    | Ok 12 -> `GT
    | Ok 13 -> `LE
    | Ok 14 -> `AL
    | _ -> raise (Defs.Lift_Error "unrecognized condition value")

  let cond_var cond = let z = var Env.zf in
    let c = var Env.cf in
    let v = var Env.vf in
    let n = var Env.nf in
    let f = b0 in
    let t = b1 in
    let open DSL.Bool in
    match cond with
    | `EQ -> `Var (z = t)
    | `NE -> `Var (z = f)
    | `CS -> `Var (c = t)
    | `CC -> `Var (c = f)
    | `MI -> `Var (n = t)
    | `PL -> `Var (n = f)
    | `VS -> `Var (v = t)
    | `VC -> `Var (v = f)
    | `HI -> `Var ((c = t) land (z = f))
    | `LS -> `Var ((c = f) lor  (z = t))
    | `GE -> `Var (n = v)
    | `LT -> `Var (n <> v)
    | `GT -> `Var ((z = f) land (n =  v))
    | `LE -> `Var ((z = t) lor  (n <> v))
    | `AL -> `Const true

  (** resolve operands to theory bool directly *)
  let resolve_cond cond = Defs.assert_imm cond |> cond_of_word |> cond_var
end