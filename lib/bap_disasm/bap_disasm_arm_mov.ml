open Core_kernel.Std
open Bap_types.Std

open Bap_disasm_arm_types
open Bap_disasm_arm_utils
open Bap_disasm_arm_flags

module Arm = Bap_disasm_arm
module Env = Bap_disasm_arm_env
module Shift = Bap_disasm_arm_shift

let lift ?dest src1 ?src2 (itype ) ?sreg ?simm raw ~wflag cond =
  let dest : var = match dest with
    | None     -> Env.new_tmp "dest"
    | Some (Op.Reg reg) -> Env.of_reg reg
    | Some (Op.Imm _) -> fail _here_ "dest is not a reg" in
  let s1 : exp = exp_of_op src1 in
  let s2 : exp = match src2 with
    | Some src -> exp_of_op src
    | None     -> zero reg32_t in

  let unshifted = Env.new_tmp "t" in

  (* Do the register shift *)
  let s1, s2, stmts, carry =
    match itype, sreg, simm with
    | `MOV, Some sreg, Some simm
    | `MVN, Some sreg, Some simm ->
      let shifted, carry = Shift.lift_r
          ~src:Bil.(var unshifted) simm
          ~shift:(exp_of_op sreg) reg32_t in
      shifted, s2, [Bil.move unshifted s1], carry
    | _, Some sreg, Some simm ->
      let shifted, carry = Shift.lift_r
          ~src:Bil.(var unshifted) simm
          ~shift:(exp_of_op sreg) reg32_t in
      s1, shifted, [Bil.move unshifted s2], carry
    | `MOV, None, Some simm
    | `MVN, None, Some simm ->
      let shifted, carry = Shift.lift_i
          ~src:Bil.(var unshifted) simm reg32_t in
      shifted, s2, [Bil.move unshifted s1], carry
    | _, None, Some simm ->
      let shifted, carry = Shift.lift_i
          ~src:Bil.(var unshifted) simm reg32_t in
      s1, shifted, [Bil.move unshifted s2], carry
    | _ -> s1, s2, [], Bil.var Env.cf in

  let stmts, flags = match itype, src1, src2 with
    | `MOV, Op.Imm i64, _
    | `MVN, Op.Imm i64, _
    | `AND, _,         Some (Op.Imm i64)
    | `BIC, _,         Some (Op.Imm i64)
    | `EOR, _,         Some (Op.Imm i64)
    | `ORR, _,         Some (Op.Imm i64) ->
      stmts, set_cf_data i64 raw :: set_nzf Bil.(var dest) reg32_t
    | #move, _, _ ->
      stmts, Bil.move Env.cf carry :: set_nzf Bil.(var dest) reg32_t
    | #arth as itype1, _, _ ->
      let orig1 = Env.new_tmp "s" in
      let orig2 = Env.new_tmp "t" in
      let v1,v2,vd = Bil.(var orig1, var orig2, var dest) in
      let flags = match itype1 with
        | `SUB -> set_sub v1 v2 vd reg32_t
        | `RSB -> set_sub v2 v1 vd reg32_t
        | `ADD -> set_add v1 v2 vd reg32_t
        | `ADC -> set_adc v1 v2 vd reg32_t
        | `SBC -> set_sbc v1 v2 vd reg32_t
        | `RSC -> set_sbc v2 v1 vd reg32_t in
      stmts @ [Bil.move orig1 s1; Bil.move orig2 s2], flags in
  let vcf = Bil.var Env.cf in
  let oper = match itype with
    | `AND -> Bil.(s1 land s2)
    | `BIC -> Bil.(s1 land lnot s2)
    | `EOR -> Bil.(s1 lxor s2)
    | `MOV -> s1
    | `MVN -> Bil.(lnot s1)
    | `ORR -> Bil.(s1 lor s2)
    | `SUB -> Bil.(s1 - s2)
    | `RSB -> Bil.(s2 - s1)
    | `ADD -> Bil.(s1 + s2)
    | `ADC -> Bil.(s1 + s2 + cast unsigned 32 vcf)
    | `SBC -> Bil.(s1 + lnot s2 + cast unsigned 32 vcf)
    | `RSC -> Bil.(lnot s1 + s2 + cast unsigned 32 vcf) in
  exec (stmts @ [assn dest oper]) ~flags ~wflag cond
