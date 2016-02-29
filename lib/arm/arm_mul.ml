open Core_kernel.Std
open Regular.Std
open Bap.Std

open Arm_types
open Arm_utils

module Mov   = Arm_mov
module Env   = Arm_env
module Shift = Arm_shift
module Flags = Arm_flags

let lift_mull ~lodest ~hidest ~src1 ~src2 sign ?addend ~wflag cond =
  let lodest = assert_reg _here_ lodest in
  let hidest = assert_reg _here_ hidest in
  let s1_64, s2_64 =
    let cast src = cast_of_sign sign 64 (exp_of_op src) in
    cast src1, cast src2 in
  let result = tmp reg64_t in
  let eres  = Bil.var result in
  let flags = Flags.set_nzf eres reg64_t in
  let opn = match addend with
    | Some _ -> Bil.(s1_64 * s2_64 +
                     concat (exp_of_reg hidest) (exp_of_reg lodest))
    | None   -> Bil.(s1_64 * s2_64) in
  let insns = [
    Bil.move result opn;
    Bil.move (Env.of_reg lodest) Bil.(extract 31 0 eres);
    Bil.move (Env.of_reg hidest) Bil.(extract 63 32 eres);
  ] in
  exec insns ~flags ~wflag cond

let lift_smul ~dest ?hidest ~src1 ~src2 ?accum ?hiaccum ?q size cond =
  let dest = assert_reg _here_ dest in
  let src1 = exp_of_op src1 in
  let src2 = exp_of_op src2 in
  let excast hi lo s = Bil.(cast signed 64 (extract hi lo s)) in
  let top  = excast 31 16 in
  let bot  = excast 15 0 in
  let top32 = excast 47 16 in
  let res = tmp reg64_t in
  let result =
    let open Bil in
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
    let open Bil in
    match accum, hiaccum with
    | None,   None     -> result
    | Some a, None     -> result + cast signed 64 (exp_of_op a)
    | Some a, Some hia -> result + concat (exp_of_op hia) (exp_of_op a)
    | _ -> fail _here_ "Cannot specify only a hi accumulator" in
  let qflag =
    match q with
    | Some true ->
      [Bil.move Env.qf Bil.(excast 31 0 (var res) <> (var res))]
    | _ -> [] in
  let instr =
    match hidest with
    | Some (`Reg hid) -> [
        Bil.move res result;
        Bil.move (Env.of_reg hid)  Bil.(extract 63 32 (var res));
        Bil.move (Env.of_reg dest) Bil.(extract 31 0  (var res));
      ]
    | None -> [
        Bil.move res result;
        Bil.move (Env.of_reg dest) Bil.(extract 31 0 (var res));
      ]
    | _ -> fail _here_ "unexpected operand type" in
  exec (instr @ qflag) cond
