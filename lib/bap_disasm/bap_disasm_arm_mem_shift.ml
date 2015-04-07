open Core_kernel.Std
open Bap_types.Std
open Or_error

open Bap_disasm_arm_types
open Bap_disasm_arm_utils

module Arm = Bap_disasm_arm
module Mem = Bap_disasm_arm_mem
module Env = Bap_disasm_arm_env
module Shift = Bap_disasm_arm_shift



let string_of_opt_op = function
  | None -> "unspecified"
  | Some reg -> Arm.Op.to_string reg

module Z = Word.Int_exn

let word x = Word.of_int x ~width:32


let repair_imm (src : word) ~sign_mask ~imm_mask rtype : exp =
  let bit_set =
    Word.(Z.(word sign_mask land src) = word sign_mask) in
  let negate =
    (bit_set && rtype = `NEG) ||
    (not bit_set && rtype = `POS) in
  let offset = Z.(src land word imm_mask) in
  Bil.int (if negate then Z.neg offset else offset)

let repair_reg reg imm ~sign_mask rtype =
  let bit_set =
    Word.(Z.(word sign_mask land imm) = word sign_mask) in
  let negate =
    (bit_set && rtype = `NEG) || (not bit_set && rtype = `POS)
  in
  let m_one = Word.(ones (bitwidth imm))  in
  if negate then Bil.(int m_one * reg) else reg




let lift_r_op ~dest1 ?dest2 ?shift ~base ~offset mode sign size operation =
  let base = assert_reg _here_ base |> Env.of_reg in
  let (offset : exp) =
    match offset with
    | Op.Reg r -> Bil.(var (Env.of_reg r))
    | Op.Imm w ->
      let width = Word.bitwidth w in
      let _1 = Word.one 32 in
      let min_32 = Word.Int_exn.(_1 lsl Word.of_int 31 ~width) in
      if Word.(w = min_32)
      then Bil.(int Word.(zero width))
      else Bil.(int w) in

  let offset = match shift with
    | Some s -> Shift.lift_mem ~src:offset s reg32_t
    | None -> offset in

  match dest1, dest2 with
  | (Op.Reg (#Reg.gpr as d1), Some (Op.Reg (#Reg.gpr as d2))) ->
    Mem.lift_r ~dst1:(Env.of_reg d1) ~dst2:(Env.of_reg d2)
      ~base ~offset mode sign size operation
  | Op.Reg (#Reg.gpr as d), None  ->
    Mem.lift_r ~dst1:(Env.of_reg d) ~base ~offset mode sign size
      operation
  | op1,op2 -> fail _here_ "Unexpected arguments: %s, %s"
                 (Arm.Op.to_string op1)
                 (string_of_opt_op op2)

let lift_r_exp ~dest1 ?dest2 ~base ~offset mode sign size operation =
  let dest1 = assert_reg _here_ dest1 |> Env.of_reg in
  let base = assert_reg _here_ base |> Env.of_reg in
  match dest2 with
  | Some dest2 ->
    let dest2 = assert_reg _here_ dest2 |> Env.of_reg in
    Mem.lift_r ~dst1:dest1 ~dst2:dest2
      ~base ~offset mode sign size operation
  | None ->
    Mem.lift_r ~dst1:dest1
      ~base ~offset mode sign size operation


let lift_m dest_list base mode update operation =
  let base = assert_reg _here_ base in
  let dest_list = List.map  dest_list
      ~f:(fun d -> assert_reg _here_ d |> Env.of_reg) in
  let base = Env.of_reg base in
  Mem.lift_m dest_list base mode update operation


(* Decides whether to use the register or immediate as the offset value
 * Also performs conversion to remove the negative bit and the
 **)
let mem_offset_reg_or_imm_neg reg_off imm_off =
  match reg_off with
  | Op.Reg #Reg.nil ->
    repair_imm imm_off ~sign_mask:0x100 ~imm_mask:0xff `NEG
  | Op.Reg (#Reg.gpr as reg) ->
    repair_reg Bil.(var (Env.of_reg reg)) imm_off ~sign_mask:0x100 `NEG
  | op -> fail _here_ "unexpected operand: %s" (Arm.Op.to_string op)

let mem_offset_reg_or_imm_pos reg_off imm_off =
  match reg_off with
  | Op.Reg #Reg.nil ->
    repair_imm imm_off ~sign_mask:0x100 ~imm_mask:0xff `POS
  | Op.Reg (#Reg.gpr as reg) ->
    repair_reg Bil.(var (Env.of_reg reg)) imm_off ~sign_mask:0x1 `POS
  | op -> fail _here_ "unexpected operand: %s" (Arm.Op.to_string op)
