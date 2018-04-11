open Core_kernel
open Regular.Std
open Bap.Std

open Arm_types
open Arm_utils
open Arm_flags

module Env = Arm_env
module Shift = Arm_shift


let lift ?dest src1 ?src2 (itype ) ?sreg ?simm raw ~wflag cond =
  let dest : var = match dest with
    | None     -> tmp reg32_t
    | Some (`Reg reg) -> Env.of_reg reg
    | Some (`Imm _) -> fail [%here] "dest is not a reg" in
  let s1 : exp = exp_of_op src1 in
  let s2 : exp = match src2 with
    | Some src -> exp_of_op src
    | None     -> zero reg32_t in

  let unshifted = tmp reg32_t in

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
    | `MOV, `Imm i64, _
    | `MVN, `Imm i64, _
    | `AND, _,         Some (`Imm i64)
    | `BIC, _,         Some (`Imm i64)
    | `EOR, _,         Some (`Imm i64)
    | `ORR, _,         Some (`Imm i64) ->
      stmts, set_cf_data i64 raw :: set_nzf Bil.(var dest) reg32_t
    | #move, _, _ ->
      stmts, Bil.move Env.cf carry :: set_nzf Bil.(var dest) reg32_t
    | #arth as itype1, _, _ ->
      let orig1 = tmp reg32_t in
      let orig2 = tmp reg32_t in
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
