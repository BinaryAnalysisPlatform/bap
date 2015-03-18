open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_arm_types
open Bap_disasm_arm_utils

module Mov = Bap_disasm_arm_mov
module Env = Bap_disasm_arm_env
module Shift = Bap_disasm_arm_shift
module Flags = Bap_disasm_arm_flags


let new_tmp name = Var.create ~tmp:true name reg64_t

let lift_mull ~lodest ~hidest ~src1 ~src2 sign ?addend ~wflag cond =
  let lodest = assert_reg _here_ lodest in
  let hidest = assert_reg _here_ hidest in
  let s1_64, s2_64 =
    let cast src = cast_of_sign sign 64 (exp_of_op src) in
    cast src1, cast src2 in
  let result = new_tmp "r" in
  let eres  = Exp.var result in
  let flags = Flags.set_nzf eres reg64_t in
  let opn = match addend with
    | Some _ -> Exp.(s1_64 * s2_64 +
                     concat (exp_of_reg hidest) (exp_of_reg lodest))
    | None   -> Exp.(s1_64 * s2_64) in
  let insns = [
    Stmt.move result opn;
    Stmt.move (Env.of_reg lodest) Exp.(extract 31 0 eres);
    Stmt.move (Env.of_reg hidest) Exp.(extract 63 32 eres);
  ] in
  exec insns ~flags ~wflag cond

let lift_smul ~dest ?hidest ~src1 ~src2 ?accum ?hiaccum ?q size cond =
  let dest = assert_reg _here_ dest in
  let src1 = exp_of_op src1 in
  let src2 = exp_of_op src2 in
  let excast hi lo s = Exp.(cast signed 64 (extract hi lo s)) in
  let top  = excast 31 16 in
  let bot  = excast 15 0 in
  let top32 = excast 47 16 in
  let res = new_tmp "r" in
  let result =
    let open Exp in
    match size with
    | BB -> bot src1 * bot src2
    | BT -> bot src1 * top src2
    | TB -> top src1 * bot src2
    | TT -> top src1 * top src2
    | D  -> top src1 * top src2 + bot src1 * bot src2
    | DX -> top src1 * bot src2 + bot src1 * top src2
    | WB -> top32 (cast signed 64 (src1 * bot src2))
    | WT -> top32 (cast signed 64 (src1 * top src2))  in
  let result =
    let open Exp in
    match accum, hiaccum with
    | None,   None     -> result
    | Some a, None     -> result + cast signed 64 (exp_of_op a)
    | Some a, Some hia -> result + concat (exp_of_op hia) (exp_of_op a)
    | _ -> fail _here_ "Cannot specify only a hi accumulator" in
  let qflag =
    match q with
    | Some true ->
      [Stmt.move Env.qf Exp.(excast 31 0 (var res) <> (var res))]
    | _ -> [] in
  let instr =
    match hidest with
    | Some (Op.Reg hid) -> [
        Stmt.move res result;
        Stmt.move (Env.of_reg hid)  Exp.(extract 63 32 (var res));
        Stmt.move (Env.of_reg dest) Exp.(extract 31 0  (var res));
      ]
    | None -> [
        Stmt.move res result;
        Stmt.move (Env.of_reg dest) Exp.(extract 31 0 (var res));
      ]
    | _ -> fail _here_ "unexpected operand type" in
  exec (instr @ qflag) cond
