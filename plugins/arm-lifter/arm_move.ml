open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Arm_env.Env
module Defs = Arm_defs
module Mov(Core : Theory.Core) = struct
  open Core
  module DSL = Arm_dsl.Make(Core)
  module Flags = Arm_flags.Flags(Core)
  open Flags

  let shift_of_word op = match Word.to_int op with
    | Ok 1 -> `ASR
    | Ok 2 -> `LSL
    | Ok 3 -> `LSR
    | Ok 4 -> `ROR
    | Ok 5 -> `RRX
    | _ -> raise (Defs.Lift_Error "unrecognized shifting value")

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

  let as_cond cond = let z = var Env.zf in
    let c = var Env.cf in
    let v = var Env.vf in
    let n = var Env.nf in
    let f = b0 in
    let t = b1 in
    match cond with
    | `EQ -> DSL.Bool.(z = t)
    | `NE -> DSL.Bool.(z = f)
    | `CS -> DSL.Bool.(c = t)
    | `CC -> DSL.Bool.(c = f)
    | `MI -> DSL.Bool.(n = t)
    | `PL -> DSL.Bool.(n = f)
    | `VS -> DSL.Bool.(v = t)
    | `VC -> DSL.Bool.(v = f)
    | `HI -> DSL.Bool.((c = t) land (z = f))
    | `LS -> DSL.Bool.((c = f) lor  (z = t))
    | `GE -> DSL.Bool.(n = v)
    | `LT -> DSL.Bool.(n <> v)
    | `GT -> DSL.Bool.((z = f) land (n =  v))
    | `LE -> DSL.Bool.((z = t) lor  (n <> v))
    | `AL -> t 

  let is_cpsr = function
    | `Reg `CPSR -> true
    | _ -> false

  (* wflag is valid only when it's (`Reg `CPSR) *)
  let movi dest src cond wflag = 
    DSL.[
      assert_var dest := assert_val src;
      when_ (is_cpsr wflag) [
        set_nzf (assert_var dest)
      ]
    ]

  (* this has exactly the same code as with `movi`, except the src variant is different *)
  let movr dest src cond wflag =
    DSL.[
      assert_var dest := assert_val src;
      when_ (is_cpsr wflag) [
        set_nzf (assert_var dest)
      ]
    ]

  let movsr dest src sreg simm cond wflag =
    DSL.[

    ]

end