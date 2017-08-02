open Core_kernel.Std
open Bap.Std
open Or_error

open Riscv_types
open Riscv_utils

module Env = Riscv_env

let pc_offset = Word.(of_int 4 ~width:32) (* 32bit for now *)
let word = Word.of_int ~width:32

let lift operand1 operand2 ?cond ?link addr =
  let cond =
    match operand1, operand2 with
    | `Reg r_1, `Reg r_2 -> Bil.[]
    | `Reg r_1, `Imm imm -> Bil.[]
    | `Imm imm, `Reg r_2 -> Bil.[]
    | `Imm imm1, `Imm imm2 -> Bil.[]

  Bil.If (c, [Bil.jmp addr])
  let target =
    match operand1 with
    | `Reg r -> Bil.var (Env.of_reg r)
    | `Imm offset ->
      let width = Word.bitwidth offset in
      let _1 = Word.one 32 in
      let min_32 = Word.Int_exn.(_1 lsl Word.of_int 31 ~width) in
      let offset = if offset = min_32 then Word.zero 32 else offset in
      let r = Word.Int_exn.(addr + pc_offset + offset) in
      Bil.int r in
  let jump_instr = [Bil.jmp target] in
  let link_instr =
    let next_addr = Word.Int_exn.(addr + pc_offset - word 4) in
    match link with
    | Some true -> [Bil.move Env.ra Bil.(int next_addr)]
    | _         -> [] in
  let stmts = link_instr @ jump_instr in
  match cond with
  | Some c -> exec stmts c
  | None -> stmts
