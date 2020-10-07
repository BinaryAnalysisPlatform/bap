open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Defs = Thumb_defs

module Branch(Core : Theory.Core) = struct
  open Core

  module Utils = Thumb_util.Utils(Core)
  module DSL = Thumb_dsl.Make(Core)

  open Utils

  let tbcc cond target addr = match cond, target with
    | `Imm cond, `Imm _ ->
      let z = var Env.zf in
      let c = var Env.cf in
      let v = var Env.vf in
      let n = var Env.nf in
      let eq_ a b = or_ (and_ a b) (and_ (inv a) (inv b)) in
      let cond = Bap.Std.Word.to_int cond |> Result.ok |> Option.value_exn |> Defs.of_int_exn in
      let always_true = match cond with
        | `AL -> true
        | _ -> false in
      let cond_val = match cond with
        | `EQ -> z
        | `NE -> inv z
        | `CS -> c
        | `CC -> inv c
        | `MI -> n
        | `PL -> inv n
        | `VS -> v
        | `VC -> inv v
        | `HI -> and_ c (inv z)
        | `LS -> or_ (inv c) z
        | `GE -> eq_ n v
        | `LT -> eq_ n v |> inv
        | `GT -> and_ (inv z) (eq_ n v)
        | `LE -> or_ z (eq_ n v |> inv)
        | `AL -> b1 in
      let jump_address = DSL.(addr + !$target + !!2) in
      let eff_hold = (jmp jump_address, pass) in
      if always_true then eff_hold
      else (
        branch cond_val (fst eff_hold) skip, (* control effect branch *)
        pass  (* data effect branch *)
      )
    | _ -> failwith "operands must be immediate"

  let tb target addr =
    (DSL.(
        jmp (!$target + addr + !!2)
      ), pass)

  let tbl target addr =
    (DSL.(
        jmp (addr + !$target + !!4)
      ), DSL.(
        Env.lr := (addr - !!2) lor !!1
      ))

  (* TODO : switch to normal mode *)
  let tblxi target addr =
    (DSL.(
        jmp ((addr + !$target + !!4) land !!0xfffffffc)
      ), DSL.(
        Env.lr := (addr - !!2) lor !!1
      ))

  let tblxr target addr =
    (DSL.(
        jmp (!$+target land !!0xfffffffe)
      ), DSL.(
        Env.lr := (addr - !!2) lor !!1
      ))

  let tbx target =
    (DSL.(
        (* reference here is PC = Rm[31:1] << 1 *)
        jmp (extract Env.value !!31 !!1 !$+target << !!1)
      ), pass)

end
