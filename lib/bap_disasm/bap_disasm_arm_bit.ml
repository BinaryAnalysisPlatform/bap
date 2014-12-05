open Core_kernel.Std
open Bap_types.Std
open Or_error

open Bap_disasm_arm_types
open Bap_disasm_arm_utils

module Env = Bap_disasm_arm_env
module Shift = Bap_disasm_arm_shift

let bits_of_size = function
  | `H -> 16
  | `B -> 8


let wordm x = Ok (Word.of_int x ~width:32)

let extend ~dest ~src ?src2 sign size ~rot cond =
  let rot = assert_imm _here_ rot in
  let dest = assert_reg _here_ dest in
  let amount = match Word.Int.((!$rot * wordm 8)) with
    | Ok amount -> amount
    | Error err -> fail _here_ "failed to obtain amount" in
  let rotated, (_ : exp) =
    if Word.is_zero amount then
      exp_of_op src, Exp.int (Word.zero 32)
    else
      Shift.lift_c ~src:(exp_of_op src)
        `ROR ~shift:(Exp.int amount) reg32_t in
  let extracted =
    Exp.(cast Cast.low (bits_of_size size) rotated) in
  let extent = cast_of_sign sign 32 extracted in
  let final = match src2 with
    | Some s2 -> Exp.(exp_of_op s2 + extent)
    | None    -> extent in
  exec [assn (Env.of_reg dest) final] cond

let bit_extract ~dest ~src sign ~lsb ~widthminus1 cond =
  let dest = assert_reg _here_ dest in
  let lsb = assert_imm _here_ lsb in
  let widthminus1 = assert_imm _here_ widthminus1 in
  let int_of_imm imm = match Word.to_int imm with
    | Ok imm -> imm
    | Error err -> fail _here_ "can't cast word to int: %s" @@
      Error.to_string_hum err  in
  let low = int_of_imm lsb in
  let high = low + (int_of_imm widthminus1) in
  let extracted = Exp.extract high low (exp_of_op src) in
  let final = cast_of_sign sign 32 extracted in
  exec [assn (Env.of_reg dest) final] cond

let get_lsb_width instr : int * int =
  let open Word.Int_exn in
  let width = Word.bitwidth instr in
  let (!$) = Word.of_int ~width in
  let lsb = (instr lsr !$7) land !$0x1f in
  let msb = (instr lsr !$16) land !$0x1f in
  let width = abs (msb - lsb + !$1) in
  match Word.(to_int lsb, to_int width) with
  | Ok lsb, Ok width -> lsb,width
  | _ -> fail _here_ "failed to get_lsb_width"

let bit_field_insert ~dest ~src raw cond =
  let dest = assert_reg _here_ dest in
  let d   = Env.of_reg dest in
  let d_e = Exp.var d in
  let lsb, width = get_lsb_width raw in
  let extracted = Exp.extract (width - 1) 0 (exp_of_op src) in
  let ext_h b s = Exp.extract 31 b s in
  let ext_l b s = Exp.extract b 0 s in
  let inst = match lsb + width - 1, lsb with
    | 31, 0 -> extracted
    | 31, l -> Exp.concat extracted (ext_l (l - 1) d_e)
    | m,  0 -> Exp.concat (ext_h (m + 1) d_e) extracted
    | m,  l -> Exp.concat (Exp.concat
                             (ext_h (m + 1) d_e) extracted)
                 (ext_l (l - 1) d_e) in
  exec [Stmt.move d inst] cond
