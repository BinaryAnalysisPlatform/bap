open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Arm_env.Env
module Defs = Arm_defs


module Branch(Core : Theory.Core) = struct
  open Core
  module DSL = Arm_dsl.Make(Core)
  module Flags = Arm_flags.Flags(Core)
  module Shift = Arm_shift.Shift(Core)
  module Cond = Arm_cond.Cond(Core)
  module Var = Theory.Var
  open Flags
  open Cond

  let target_address offset addr =
    match offset with
    | `Reg _ -> DSL.assert_val offset
    | `Imm offset -> 
      let int32_min = Word.(one 32 lsl Word.of_int 31 32) in
      let offset = 
        if Word.equal offset int32_min 
        then DSL.imm 0 
        else DSL.word_as_bitv offset in
      DSL.(addr + var Env.pc + offset)

  let bcc offset cond addr =
    let target = target_address offset addr in
    DSL.[
      if_ (resolve_cond cond) [
        Env.pc := target;
        (* TODO : `jmp target` here *)
      ]
    ]

  let bl offset cond addr =
    let target = target_address offset addr in
    DSL.[
      if_ (resolve_cond cond) [
        Env.lr := addr + var Env.pc - !!4;
        Env.pc := target;
      ]
    ]

  let bl_pred offset cond addr =
    let target = target_address offset addr in
    DSL.[
      if_ (resolve_cond cond) [
        Env.lr := addr + var Env.pc - !!4;
        Env.pc := target;
      ]
    ]

  (* TODO : switch to thumb mode *)
  let bx_ret cond =
    DSL.[
      if_ (resolve_cond cond) [
        Env.pc := var Env.lr
      ]
    ]

  let bx target addr =
    let target = target_address target addr in
    DSL.[
      Env.pc := target
    ]

  let bx_pred target cond addr =
    let target = target_address target addr in
    DSL.[
      if_ (resolve_cond cond) [
        Env.pc := target
      ]
    ]

  let blx target addr =
    let target = target_address target addr in
    DSL.[
      Env.lr := addr + var Env.pc - !!4;
      Env.pc := target
    ]

  let blx_pred target cond addr =
    let target = target_address target addr in
    DSL.[
      if_ (resolve_cond cond) [
        Env.lr := addr + var Env.pc - !!4;
        Env.pc := target
      ]
    ]

  let blxi offset addr =
    let target = target_address offset addr in
    DSL.[
      Env.lr := addr + var Env.pc - !!4;
      Env.pc := target
    ]

end