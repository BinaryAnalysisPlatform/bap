open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Armng_env.Env
module Defs = Armng_defs


module Branch(Core : Theory.Core) = struct
  open Core
  module DSL = Armng_dsl.Make(Core)
  module Flags = Armng_flags.Flags(Core)
  module Shift = Armng_shift.Shift(Core)
  module Cond = Armng_cond.Cond(Core)
  module Var = Theory.Var
  open Flags
  open Cond

  let target_address offset addr =
    let addr = Core.int Env.value addr in
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
    DSL.(
      if_ (resolve_cond cond) [
        ctrl addr @@ jmp target
      ])

  let bl offset cond addr =
    let target = target_address offset addr in
    let addr_val = Core.int Env.value addr in
    DSL.(
      if_ (resolve_cond cond) [
        Env.lr := addr_val + var Env.pc - !!4;
        ctrl addr @@ jmp target
      ])

  let bl_pred offset cond addr =
    let target = target_address offset addr in
    let addr_val = Core.int Env.value addr in
    DSL.(
      if_ (resolve_cond cond) [
        Env.lr := addr_val + var Env.pc - !!4;
        ctrl addr @@ jmp target
      ])

  (* TODO : switch to thumb mode *)
  let bx_ret cond addr =
    DSL.(
      if_ (resolve_cond cond) [
        ctrl addr @@ jmp (var Env.lr)
      ])

  let bx target addr =
    let target = target_address target addr in
    DSL.(
      ctrl addr @@ jmp target
    )

  let bx_pred target cond addr =
    let target = target_address target addr in
    DSL.(
      if_ (resolve_cond cond) [
        ctrl addr @@ jmp target
      ])

  let blx target addr =
    let target = target_address target addr in
    let addr_val = Core.int Env.value addr in
    DSL.(
      if_ (`Const true) [
        Env.lr := addr_val + var Env.pc - !!4;
        ctrl addr @@ jmp target;
      ]
    )

  let blx_pred target cond addr =
    let target = target_address target addr in
    let addr_val = Core.int Env.value addr in
    DSL.(
      if_ (resolve_cond cond) [
        Env.lr := addr_val + var Env.pc - !!4;
        ctrl addr @@ jmp target
      ])

  let blxi offset addr =
    let target = target_address offset addr in
    let addr_val = Core.int Env.value addr in
    DSL.(
      if_ (`Const true) [
        Env.lr := addr_val + var Env.pc - !!4;
        ctrl addr @@ jmp target;
      ]
    )

end