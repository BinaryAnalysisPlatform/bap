open Bap_core_theory
open Base
open KB.Syntax
open KB

module Defs = Armng_defs
module Flags = Armng_flags.Flags
module Insns = Armng_insn

let package = "arm-lifter"

type insns = Defs.insn * (Defs.op list)

module ARM(Core : Theory.Core) = struct
  open Core
  open Defs
  module Env = Armng_env.Env
  module FP = Armng_env_fp.Env_fp
  module Utils = Armng_util.Utils(Core)
  module Mov = Armng_move.Mov(Core)
  module Bits = Armng_bits.Bits(Core)
  module Mul = Armng_mul.Mul(Core)
  module Special = Armng_special.Special(Core)
  module Mem = Armng_mem.Mem(Core)
  module Mem_multi = Armng_mem.Mem_Multi(Core)
  module Branch = Armng_branch.Branch(Core)
  module DSL = Armng_dsl.Make(Core)

  open Utils

  let move eff =
    KB.Object.create Theory.Program.cls >>= fun lbl ->
    blk lbl eff skip

  let ctrl eff data pc = 
    Theory.Label.for_addr pc >>= fun lbl ->
    blk lbl data eff

  let lift_move (insn : Defs.insn ) ops address =
    let ( !% ) list = DSL.expand list |> move in
    let open Mov in
    match insn, ops with
    | `MOVi,  [|dest; src; cond; _; wflag|] -> 
      !%(movi dest src cond wflag)
    | `MOVr,  [|dest; src; cond; _; wflag|] ->
      !%(movr dest src cond wflag)
    | `MOVsr, [|dest; src; sreg; simm; cond; _; wflag|] ->
      !%(movsr dest src sreg simm cond wflag)
    | `MOVsi, [|dest; src; shift_imm; cond; _; wflag|] ->
      !%(movsi dest src shift_imm cond wflag)
    | `MOVPCLR, [|cond; wflag|] ->
      let data_eff, ctrl_eff = movpclr cond wflag in
      ctrl ctrl_eff (DSL.expand data_eff) address
    | `MVNi, [|dest; src; cond; _; wflag|] ->
      !%(mvni dest src cond wflag)
    | `MVNr, [|dest; src; cond; _; wflag|] ->
      !%(mvnr dest src cond wflag)
    | `MVNsr, [|dest; src; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(mvnsr dest src shift_reg shift_imm cond wflag)
    | `MVNsi, [|dest; src; shift_imm; cond; _; wflag|] ->
      !%(mvnsi dest src shift_imm cond wflag)
    | `ANDri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(andri dest src1 src2 cond wflag)
    | `ANDrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(andrr dest src1 src2 cond wflag)
    | `ANDrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(andrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ANDrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(andrsi dest src1 src2 shift_imm cond wflag)
    | `BICri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(bicri dest src1 src2 cond wflag)
    | `BICrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(bicrr dest src1 src2 cond wflag)
    | `BICrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(bicrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `BICrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(bicrsi dest src1 src2 shift_imm cond wflag)
    | `EORri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(eorri dest src1 src2 cond wflag)
    | `EORrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(eorrr dest src1 src2 cond wflag)
    | `EORrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(eorrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `EORrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(eorrsi dest src1 src2 shift_imm cond wflag)
    | `ORRri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(orrri dest src1 src2 cond wflag)
    | `ORRrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(orrrr dest src1 src2 cond wflag)
    | `ORRrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(orrrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ORRrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(orrrsi dest src1 src2 shift_imm cond wflag)
    | `TEQri, [|src1; src2; cond; _|] ->
      !%(teqri src1 src2 cond)
    | `TEQrr, [|src1; src2; cond; _|] ->
      !%(teqrr src1 src2 cond)
    | `TEQrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(teqrsr src1 src2 shift_reg shift_imm cond)
    | `TEQrsi, [|_dest; src1; src2; shift_imm; cond; _|] ->
      !%(teqrsi src1 src2 shift_imm cond)
    | `TSTri, [|src1; src2; cond; _|] ->
      !%(tstri src1 src2 cond)
    | `TSTrr, [|src1; src2; cond; _|] ->
      !%(tstrr src1 src2 cond)
    | `TSTrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(tstrsr src1 src2 shift_reg shift_imm cond)
    | `TSTrsi, [|src1; src2; shift_imm; cond; _|] ->
      !%(tstrsi src1 src2 shift_imm cond)
    | `ADDri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(addri dest src1 src2 cond wflag)
    | `ADDrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(addrr dest src1 src2 cond wflag)
    | `ADDrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(addrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ADDrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(addrsi dest src1 src2 shift_imm cond wflag)
    | `SUBri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(subri dest src1 src2 cond wflag)
    | `SUBrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(subrr dest src1 src2 cond wflag)
    | `SUBrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(subrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `SUBrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(subrsi dest src1 src2 shift_imm cond wflag)
    | `ADCri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(adcri dest src1 src2 cond wflag)
    | `ADCrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(adcrr dest src1 src2 cond wflag)
    | `ADCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(adcrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `ADCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(adcrsi dest src1 src2 shift_imm cond wflag)
    | `SBCri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(sbcri dest src1 src2 cond wflag)
    | `SBCrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(sbcrr dest src1 src2 cond wflag)
    | `SBCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(sbcrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `SBCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(sbcrsi dest src1 src2 shift_imm cond wflag)
    | `RSBri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rsbri dest src1 src2 cond wflag)
    | `RSBrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rsbrr dest src1 src2 cond wflag)
    | `RSBrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(rsbrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `RSBrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(rsbrsi dest src1 src2 shift_imm cond wflag)
    | `RSCri, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rscri dest src1 src2 cond wflag)
    | `RSCrr, [|dest; src1; src2; cond; _; wflag|] ->
      !%(rscrr dest src1 src2 cond wflag)
    | `RSCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
      !%(rscrsr dest src1 src2 shift_reg shift_imm cond wflag)
    | `RSCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
      !%(rscrsi dest src1 src2 shift_imm cond wflag)
    | `CMPri, [|src1; src2; cond; _|] ->
      !%(cmpri src1 src2 cond)
    | `CMPrr, [|src1; src2; cond; _|] ->
      !%(cmprr src1 src2 cond)
    | `CMPrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(cmprsr src1 src2 shift_reg shift_imm cond)
    | `CMPrsi, [|src1; src2; shift_imm; cond; _|] ->
      !%(cmprsi src1 src2 shift_imm cond)
    | `CMNri, [|src1; src2; cond; _|] ->
      !%(cmnri src1 src2 cond)
    | `CMNzrr, [|src1; src2; cond; _|] ->
      !%(cmnzrr src1 src2 cond)
    | `CMNzrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
      !%(cmnzrsr src1 src2 shift_reg shift_imm cond)
    | `CMNzrsi, [|src1; src2; shift_imm; cond; _|] ->
      !%(cmnzrsi src1 src2 shift_imm cond)
    (* Special Data Instructions *)
    | `MOVi16, [|`Reg dest; src; cond; _wflag|] ->
      !%(movi16 (`Reg dest) src cond _wflag)
    | `MOVTi16, [|`Reg dest; _; src; cond; _wflag|] ->
      !%(movti16 (`Reg dest) src cond _wflag)
    | _, _ -> move pass

  let lift_bits insn ops =
    let ( !% ) list = DSL.expand list |> move in
    let open Bits in
    match insn, ops with
    | `UXTB, [|dest; src; rot; cond; _|] ->
      !%(uxtb dest src rot cond)
    | `UXTH, [|dest; src; rot; cond; _|] ->
      !%(uxth dest src rot cond)
    | `SXTB, [|dest; src; rot; cond; _|] ->
      !%(sxtb dest src rot cond)
    | `SXTH, [|dest; src; rot; cond; _|] ->
      !%(sxth dest src rot cond)
    | `UXTAB, [|dest; src; shift; rot; cond; _|] ->
      !%(uxtab dest src shift rot cond)
    | `UXTAH, [|dest; src; shift; rot; cond; _|] ->
      !%(uxtah dest src shift rot cond)
    | `SXTAB, [|dest; src; shift; rot; cond; _|] ->
      !%(sxtab dest src shift rot cond)
    | `SXTAH, [|dest; src; shift; rot; cond; _|] ->
      !%(sxtah dest src shift rot cond)
    (* extracts *)
    | `UBFX, [|dest; src; lsb; widthminus1; cond; _|] ->
      !%(ubfx dest src lsb widthminus1 cond)
    | `SBFX, [|dest; src; lsb; widthminus1; cond; _|] ->
      !%(sbfx dest src lsb widthminus1 cond)
    | `BFI, [|dest; _unknown; src; _bmask; cond; _|] ->
      !%(bfi dest _unknown src _bmask cond)
    | `BFC, [|dest; _unknown; _bmask; cond; _|] ->
      !%(bfc dest _unknown _bmask cond)
    | `RBIT, [|dest; src; cond; _|] ->
      !%(rbit dest src cond)
    | `SWPB, [|`Reg dest; `Reg src1; `Reg src2; cond; _|] ->
      !%(swpb (`Reg dest) (`Reg src1) (`Reg src2) cond)
    | `PKHTB, [|`Reg dest; src1; src2; shift; cond; _|] ->
      !%(pkhtb (`Reg dest) src1 src2 shift cond)
    | `REV, [|`Reg dest; src; cond; _|] ->
      !%(rev (`Reg dest) src cond)
    | `REV16, [|`Reg dest; src; cond; _|] ->
      !%(rev16 (`Reg dest) src cond)
    | `CLZ, [|`Reg dest; src; cond; _|] ->
      !%(clz (`Reg dest) src cond)
    | _, _ -> move pass

  let lift_mult insn ops =
    let ( !% ) list = DSL.expand list |> move in
    let open Mul in
    match insn,ops with
    | `MUL, [|`Reg dest; src1; src2; cond; _rflag; wflag|] ->
      !%(mul (`Reg dest) src1 src2 cond _rflag wflag)
    | `MLA, [|`Reg dest; src1; src2; addend; cond; _rflag; wflag|] ->
      !%(mla (`Reg dest) src1 src2 addend cond _rflag wflag)
    | `MLS, [|`Reg dest; src1; src2; addend; cond; _|] ->
      !%(mls (`Reg dest) src1 src2 addend cond)
    | `UMULL, [|lodest; hidest; src1; src2; cond; _rflag; wflag|] ->
      !%(umull lodest hidest src1 src2 cond _rflag wflag)
    | `SMULL, [|lodest; hidest; src1; src2; cond; _rflag; wflag|] ->
      !%(smull lodest hidest src1 src2 cond _rflag wflag)
    | `UMLAL, [|lodest; hidest; src1; src2;
                _loadd; _hiadd; cond; _rflag; wflag|] ->
      !%(umlal lodest hidest src1 src2 cond _rflag wflag)
    | `SMLAL, [|lodest; hidest; src1; src2;
                _loadd; _hiadd; cond; _rflag; wflag|] ->
      !%(smlal lodest hidest src1 src2 cond _rflag wflag)
    (* signed 16bit mul plus a 32bit bit accum, Q *)
    | `SMLABB, [|dest; src1; src2; accum; cond; _wflag|] ->
      !%(smlabb dest src1 src2 accum cond _wflag)
    (* signed 16bit mul *)
    | `SMULBB, [|dest; src1; src2; cond; _wflag|] ->
      !%(smulbb dest src1 src2 cond _wflag)
    (* two signed 16bit muls plus 32bit accum and optional xchg, Q*)
    | `SMLAD, [|dest; src1; src2; accum; cond; _wflag|] ->
      !%(smlad dest src1 src2 accum cond _wflag)
    (* two signed 16bit muls and optional xchg, Q *)
    | `SMUAD, [|dest; src1; src2; cond; _wflag|] ->
      !%(smuad dest src1 src2 cond _wflag)
    (* signed 16bit times signed 32bit added to 32bit accum, Q *)
    | `SMLAWB, [|dest; src1; src2; accum; cond; _wflag|] ->
      !%(smlawb dest src1 src2 accum cond _wflag)
    (* signed 16bit mul *)
    | `SMULTB, [|dest; src1; src2; cond; _wflag|] ->
      !%(smultb dest src1 src2 cond _wflag)
    (* signed 16bit mul plus 64bit accum *)
    | `SMLALBT, [|dest; hidest; src1; src2; cond; _wflag|] ->
      !%(smlalbt dest hidest src1 src2 cond _wflag)
    | _, _ -> move pass

  let lift_mem insn ops =
    let ( !% ) list = DSL.expand list |> move in
    let open Mem in
    match insn, ops with
    | `STRD, [|dest1; dest2; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(strd dest1 dest2 base reg_off (`Imm imm_off) cond)
    | `LDRD, [|dest1; dest2; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrd dest1 dest2 base reg_off (`Imm imm_off) cond)
    | `STRD_POST, [|dest1; dest2; base; _unknown; reg_off; `Imm imm_off; cond; _|] ->
      !%(strd_post dest1 dest2 base _unknown reg_off (`Imm imm_off) cond)

    | `LDRD_POST, [|dest1; dest2; base; _unknown; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrd_post dest1 dest2 base _unknown reg_off (`Imm imm_off) cond)

    | `STRD_PRE, [|_unknown; dest1; dest2; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(strd_pre _unknown dest1 dest2 base reg_off (`Imm imm_off) cond)

    | `LDRD_PRE, [|dest1; dest2; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrd_pre dest1 dest2 _unknown base reg_off (`Imm imm_off) cond)
    | `STRH, [|dest1; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(strh dest1 base reg_off (`Imm imm_off) cond)
    | `LDRH, [|dest1; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrh dest1 base reg_off (`Imm imm_off) cond)
    | `STRH_PRE, [|_unknown; dest1; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(strh_pre _unknown dest1 base reg_off (`Imm imm_off) cond)
    | `LDRH_PRE, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrh_pre dest1 _unknown base reg_off (`Imm imm_off) cond)
    | `STRH_POST, [|_unknown; dest1; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(strh_post _unknown dest1 base reg_off (`Imm imm_off) cond)
    (* Unlike the convention of all other load and store instructions, for some
     * instructions the sign bit is set in the immediate when the operand
     * is POSITIVE. Insructions that are affected by this are marked with
     * "POS_SIGN_BIT"
     **)
    (* POS_SIGN_BIT *)
    | `STRHTr, [|_unknown; dest1; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(strhtr _unknown dest1 base reg_off (`Imm imm_off) cond)
    | `LDRH_POST, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrh_post dest1 _unknown base reg_off (`Imm imm_off) cond)
    (* POS_SIGN_BIT *)
    | `LDRHTr, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrhtr dest1 _unknown base reg_off (`Imm imm_off) cond)
    | `LDRSH, [|dest1; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrsh dest1 base reg_off (`Imm imm_off) cond)
    | `LDRSH_PRE, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrsh_pre dest1 _unknown base reg_off (`Imm imm_off) cond)
    | `LDRSH_POST, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrsh_post dest1 _unknown base reg_off (`Imm imm_off) cond)
    (* POS_SIGN_BIT *)
    | `LDRSHTr, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrshtr dest1 _unknown base reg_off (`Imm imm_off) cond)
    (* POS_SIGN_BIT *)
    | `LDRSHTi, [|dest1; _unknown; base; `Imm imm_off; cond; _|] ->
      !%(ldrshti dest1 _unknown base (`Imm imm_off) cond)
    | `LDRSB, [|dest1; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrsb dest1 base reg_off (`Imm imm_off) cond)
    | `LDRSB_PRE, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrsb_pre dest1 _unknown base reg_off (`Imm imm_off) cond)
    | `LDRSB_POST, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrsb_post dest1 _unknown base reg_off (`Imm imm_off) cond)
    (* POS_SIGN_BIT *)
    | `LDRSBTr, [|dest1; _unknown; base; reg_off; `Imm imm_off; cond; _|] ->
      !%(ldrsbtr dest1 _unknown base reg_off (`Imm imm_off) cond)
    | `STRi12, [|dest1; base; offset; cond; _|] ->
      !%(stri12 dest1 base offset cond)
    | `LDRi12, [|dest1; base; offset; cond; _|] ->
      !%(ldri12 dest1 base offset cond)
    | `STRBi12, [|dest1; base; offset; cond; _|] ->
      !%(strbi12 dest1 base offset cond)
    | `LDRBi12, [|dest1; base; offset; cond; _|] ->
      !%(ldrbi12 dest1 base offset cond)
    | `STRrs, [|dest1; base; offset; shift; cond; _|] ->
      !%(strrs dest1 base offset shift cond)
    | `LDRrs, [|dest1; base; offset; shift; cond; _|] ->
      !%(ldrrs dest1 base offset shift cond)
    | `STRBrs, [|dest1; base; offset; shift; cond; _|] ->
      !%(strbrs dest1 base offset shift cond)
    | `LDRBrs, [|dest1; base; offset; shift; cond; _|] ->
      !%(ldrbrs dest1 base offset shift cond)
    | `STR_POST_IMM, [|_unknown; dest1; base; _invalid; `Imm offset; cond; _|] ->
      !%(str_post_imm _unknown dest1 base _invalid (`Imm offset) cond)
    | `LDR_POST_IMM, [|dest1; _unknown; base; _invalid; `Imm offset; cond; _|] ->
      !%(ldr_post_imm dest1 _unknown base _invalid (`Imm offset) cond)
    | `STRB_POST_IMM,  [|_unknown; dest1; base; _invalid; `Imm offset; cond; _|] ->
      !%(strb_post_imm _unknown dest1 base _invalid (`Imm offset) cond)
    | `STRBT_POST_IMM, [|_unknown; dest1; base; _invalid; `Imm offset; cond; _|] ->
      !%(strbt_post_imm _unknown dest1 base _invalid (`Imm offset) cond)
    | `LDRB_POST_IMM,  [|dest1; _unknown; base; _invalid; `Imm offset; cond; _|] ->
      !%(ldrb_post_imm dest1 _unknown base _invalid (`Imm offset) cond)
    | `LDRBT_POST_IMM, [|dest1; _unknown; base; _invalid; `Imm offset; cond; _|] -> 
      !%(ldrbt_post_imm dest1 _unknown base _invalid (`Imm offset) cond)
    | `STR_POST_REG,  [|_unknown; dest1; base; offset; shift; cond; _|] ->
      !%(str_post_reg _unknown dest1 base offset shift cond)
    | `STRT_POST_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
      !%(strt_post_reg _unknown dest1 base offset shift cond)
    | `LDR_POST_REG,  [|dest1; _unknown; base; offset; shift; cond; _|] ->
      !%(ldr_post_reg dest1 _unknown base offset shift cond)
    | `LDRT_POST_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
      !%(ldrt_post_reg dest1 _unknown base offset shift cond)
    | `STRB_POST_REG,  [|_unknown; dest1; base; offset; shift; cond; _|] ->
      !%(strb_post_reg _unknown dest1 base offset shift cond)
    | `STRBT_POST_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
      !%(strbt_post_reg _unknown dest1 base offset shift cond)
    | `LDRB_POST_REG,  [|dest1; _unknown; base; offset; shift; cond; _|] ->
      !%(ldrb_post_reg dest1 _unknown base offset shift cond)
    | `LDRBT_POST_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
      !%(ldrbt_post_reg dest1 _unknown base offset shift cond)
    | `STR_PRE_IMM, [|_unknown; dest1; base; offset; cond; _|] ->
      !%(str_pre_imm _unknown dest1 base offset cond)
    | `LDR_PRE_IMM, [|dest1; _unknown; base; offset; cond; _|] ->
      !%(ldr_pre_imm dest1 _unknown base offset cond)
    | `STRB_PRE_IMM, [|_unknown; dest1; base; offset; cond; _|] ->
      !%(strb_pre_imm _unknown dest1 base offset cond)
    | `LDRB_PRE_IMM, [|dest1; _unknown; base; offset; cond; _|] ->
      !%(ldrb_pre_imm dest1 _unknown base offset cond)
    | `STR_PRE_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
      !%(str_pre_reg _unknown dest1 base offset shift cond)
    | `LDR_PRE_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
      !%(ldr_pre_reg dest1 _unknown base offset shift cond)
    | `STRB_PRE_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
      !%(strb_pre_reg _unknown dest1 base offset shift cond)
    | `LDRB_PRE_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
      !%(ldrb_pre_reg dest1 _unknown base offset shift cond)
    (* Exclusive access, we may later want to do something special to these *)
    | `LDREX, [|dest1; base; cond; _|] ->
      !%(ldrex dest1 base cond)
    | `LDREXB, [|dest1; base; cond; _|] ->
      !%(ldrexb dest1 base cond)
    | `LDREXH, [|dest1; base; cond; _|] ->
      !%(ldrexh dest1 base cond)
    (* multidest is one of the multireg combinations *)
    | `LDREXD, [|multidest; base; cond; _|] ->
      !%(ldrexd multidest base cond)
    | `STREX, [|`Reg dest1; src1; base; cond; _|] ->
      !%(strex (`Reg dest1) src1 base cond)
    | `STREXB, [|`Reg dest1; src1; base; cond; _|] ->
      !%(strexb (`Reg dest1) src1 base cond)
    | `STREXH, [|`Reg dest1; src1; base; cond; _|] ->
      !%(strexh (`Reg dest1) src1 base cond)
    (* multisrc is one of the multireg combinations *)
    | `STREXD, [|`Reg dest1; multisrc; base; cond; _|] ->
      !%(strexd (`Reg dest1) multisrc base cond)
    | _, _ -> move pass

  (** Branching instructions *)

  let lift_branch insn ops addr =
    let addr = Core.int Env.value addr in
    let open Branch in
    match insn, ops with
    | `Bcc, [|offset; cond; _|] ->
      bcc offset cond addr
    | `BL, [|offset; cond; _|] ->
      bl offset cond addr
    | `BL_pred, [|offset; cond; _|] ->
      bl_pred offset cond addr
    | `BX_RET, [|cond; _|] ->
      bx_ret cond
    | `BX, [|target|] ->
      bx target addr
    | `BX_pred, [|target; cond; _|] ->
      bx_pred target cond addr
    | `BLX, [|target|] ->
      blx target addr
    | `BLX_pred, [|target; cond; _|] ->
      blx_pred target cond addr
    | `BLXi, [|offset|] ->
      blxi offset addr
    | _, _ -> (skip, pass)

  let lift_special insn ops =
    let ( !% ) list = DSL.expand list |> move in
    let open Special in
    match insn, ops with
    | `SVC, [|`Imm word; cond; _|] ->
      !%(svc (`Imm word) cond)
    | `MRS, [|`Reg dest; cond; _|] ->
      !%(mrs (`Reg dest) cond)
    | `MSR, [|`Imm imm; `Reg src; cond; _|] ->
      !%(msr (`Imm imm) (`Reg src) cond)
    | _, _ -> move pass

  let lift_with (addr : Bitvec.t) (insn : Armng_defs.insn)
      (ops : Armng_defs.op array) = match insn with
    | #move_insn -> lift_move insn ops addr
    | #mem_insn -> lift_mem insn ops
    | #bits_insn -> lift_bits insn ops
    | #mult_insn -> lift_mult insn ops
    | #special_insn -> lift_special insn ops
    (* this is malformed for now *)
    | #branch_insn -> 
      let ctrl_eff, data_eff = lift_branch insn ops addr in
      ctrl ctrl_eff data_eff addr

end

open Bap.Std

let run_lifter _label addr insn _mem 
    (lifter : Bitvec.t -> Defs.insn -> Defs.op array -> unit Theory.eff) =
  match Insns.of_basic insn with
  | None -> raise (Defs.Lift_Error "unknown instruction")
  | Some arm_insn -> 
    match Insns.arm_ops (Disasm_expert.Basic.Insn.ops insn) with
    | Error err -> raise (Defs.Lift_Error (Error.to_string_hum err))
    | Ok ops -> lifter addr arm_insn ops

let () =
  KB.promise Theory.Program.Semantics.slot @@ fun label ->
  Theory.instance () >>= Theory.require >>= fun (module Core) ->
  KB.collect Disasm_expert.Basic.Insn.slot label >>= fun insn -> (* the LLVM provided decoding *)
  KB.collect Memory.slot label >>= fun mem -> (* the memory chunk, probably not needed *)
  let module Lifter = ARM(Core) in
  match insn, mem with
  | Some insn, Some mem ->
    let addr = Word.to_bitvec@@Memory.min_addr mem in
    run_lifter label addr insn mem Lifter.lift_with
  | _ -> KB.return Insn.empty
