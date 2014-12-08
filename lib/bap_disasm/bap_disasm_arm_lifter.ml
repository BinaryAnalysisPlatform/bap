open Core_kernel.Std
open Or_error
open Bap_types.Std

open Bap_disasm_arm_types
open Bap_disasm_arm_utils

module Basic = Bap_disasm_basic
module Arm = Bap_disasm_arm
module Bit = Bap_disasm_arm_bit
module Branch = Bap_disasm_arm_branch
module Env = Bap_disasm_arm_env
module Mem = Bap_disasm_arm_mem
module Mem_shift = Bap_disasm_arm_mem_shift
module Mov = Bap_disasm_arm_mov
module Mul = Bap_disasm_arm_mul
module Shift = Bap_disasm_arm_shift
module Flags = Bap_disasm_arm_flags

open Op

let word = Word.of_int ~width:32
let int32 x = Exp.int (word x)


let string_of_ops ops =
  Format.asprintf "%a" Sexp.pp (sexp_of_array sexp_of_op ops)

let lift_move mem ops (insn : Arm.Insn.move) : stmt list =
  let open Mov in
  match insn, ops with
  | `MOVi,  [|dest; src; cond; _; wflag|]
  | `MOVr,  [|dest; src; cond; _; wflag|] ->
    lift ~dest src `MOV mem cond ~wflag
  | `MOVsr, [|dest; src; sreg; simm; cond; _; wflag|] ->
    lift ~dest src `MOV mem cond ~wflag ~sreg ~simm
  | `MOVsi, [|dest; src; shift_imm; cond; _; wflag|] ->
    lift ~dest src `MOV ~simm:shift_imm mem cond ~wflag

  | `MVNi, [|dest; src; cond; _; wflag|]
  | `MVNr, [|dest; src; cond; _; wflag|] ->
    lift ~dest src `MVN mem cond ~wflag

  | `MVNsr, [|dest; src; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src `MVN ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `MVNsi, [|dest; src; shift_imm; cond; _; wflag|] ->
    lift ~dest src `MVN ~simm:shift_imm mem cond ~wflag

  | `ANDri, [|dest; src1; src2; cond; _; wflag|]
  | `ANDrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `AND mem cond ~wflag

  | `ANDrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `AND ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `ANDrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `AND ~simm:shift_imm
      mem cond ~wflag

  | `BICri, [|dest; src1; src2; cond; _; wflag|]
  | `BICrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `BIC mem cond ~wflag

  | `BICrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `BIC ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `BICrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `BIC ~simm:shift_imm
      mem cond ~wflag

  | `EORri, [|dest; src1; src2; cond; _; wflag|]
  | `EORrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `EOR mem cond ~wflag

  | `EORrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `EOR ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `EORrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `EOR ~simm:shift_imm
      mem cond ~wflag

  | `ORRri, [|dest; src1; src2; cond; _; wflag|]
  | `ORRrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ORR mem cond ~wflag

  | `ORRrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ORR ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `ORRrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ORR ~simm:shift_imm
      mem cond ~wflag

  | `TEQri, [|src1; src2; cond; _|]
  | `TEQrr, [|src1; src2; cond; _|] ->
    lift src1 ~src2 `EOR mem cond ~wflag:(Reg `CPSR)

  | `TEQrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
    lift src1 ~src2 `EOR ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  | `TEQrsi, [|_dest; src1; src2; shift_imm; cond; _|] ->
    lift src1 ~src2 `EOR ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  | `TSTri, [|src1; src2; cond; _|]
  | `TSTrr, [|src1; src2; cond; _|] ->
    lift src1 ~src2 `AND mem cond ~wflag:(Reg `CPSR)

  | `TSTrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
    lift src1 ~src2 `AND ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  | `TSTrsi, [|src1; src2; shift_imm; cond; _|] ->
    lift src1 ~src2 `AND ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  | `ADDri, [|dest; src1; src2; cond; _; wflag|]
  | `ADDrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ADD mem cond ~wflag

  | `ADDrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ADD ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `ADDrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ADD ~simm:shift_imm
      mem cond ~wflag

  | `SUBri, [|dest; src1; src2; cond; _; wflag|]
  | `SUBrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `SUB mem cond ~wflag

  | `SUBrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `SUB ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `SUBrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `SUB ~simm:shift_imm
      mem cond ~wflag

  | `ADCri, [|dest; src1; src2; cond; _; wflag|]
  | `ADCrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ADC mem cond ~wflag

  | `ADCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ADC ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `ADCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `ADC ~simm:shift_imm
      mem cond ~wflag

  | `SBCri, [|dest; src1; src2; cond; _; wflag|]
  | `SBCrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `SBC mem cond ~wflag

  | `SBCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `SBC ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `SBCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `SBC ~simm:shift_imm
      mem cond ~wflag

  | `RSBri, [|dest; src1; src2; cond; _; wflag|]
  | `RSBrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `RSB mem cond ~wflag

  | `RSBrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `RSB ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `RSBrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `RSB ~simm:shift_imm
      mem cond ~wflag

  | `RSCri, [|dest; src1; src2; cond; _; wflag|]
  | `RSCrr, [|dest; src1; src2; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `RSC mem cond ~wflag

  | `RSCrsr, [|dest; src1; src2; shift_reg; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `RSC ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag

  | `RSCrsi, [|dest; src1; src2; shift_imm; cond; _; wflag|] ->
    lift ~dest src1 ~src2 `RSC ~simm:shift_imm
      mem cond ~wflag

  | `CMPri, [|src1; src2; cond; _|]
  | `CMPrr, [|src1; src2; cond; _|] ->
    lift src1 ~src2 `SUB mem cond ~wflag:(Reg `CPSR)

  | `CMPrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
    lift src1 ~src2 `SUB ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  | `CMPrsi, [|src1; src2; shift_imm; cond; _|] ->
    lift src1 ~src2 `SUB ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  | `CMNri, [|src1; src2; cond; _|]
  | `CMNzrr, [|src1; src2; cond; _|] ->
    lift src1 ~src2 `ADD mem cond ~wflag:(Reg `CPSR)

  | `CMNzrsr, [|src1; src2; shift_reg; shift_imm; cond; _|] ->
    lift src1 ~src2 `ADD ~sreg:shift_reg ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  | `CMNzrsi, [|src1; src2; shift_imm; cond; _|] ->
    lift src1 ~src2 `ADD ~simm:shift_imm
      mem cond ~wflag:(Reg `CPSR)

  (** Special Data Instructions *)

  | `MOVi16, [|Reg dest; src; cond; _wflag|] ->
    exec [Stmt.move (Env.of_reg dest) (exp_of_op src)] cond

  | `MOVTi16, [|Reg dest; _; src; cond; _wflag|] ->
    let dest = Env.of_reg dest in
    [Stmt.move dest Exp.(var dest lor exp_of_op src lsl int32 16)] |>
    fun ins -> exec ins cond
  | insn,ops ->
    fail _here_ "ops %s doesn't match move insn %s"
      (string_of_ops ops) (Arm.Insn.to_string (insn :> insn))


let lift_bits mem ops (insn : Arm.Insn.bits ) =
  let open Bit in
  match insn, ops with
  (* extends *)
  | `UXTB, [|dest; src; rot; cond; _|] ->
    extend ~dest ~src Unsigned `B ~rot cond

  | `UXTH, [|dest; src; rot; cond; _|] ->
    extend ~dest ~src Unsigned `H ~rot cond

  | `SXTB, [|dest; src; rot; cond; _|] ->
    extend ~dest ~src Signed `B ~rot cond

  | `SXTH, [|dest; src; rot; cond; _|] ->
    extend ~dest ~src Signed `H ~rot cond

  | `UXTAB, [|dest; src; shift; rot; cond; _|] ->
    extend ~dest ~src:shift ~src2:src Unsigned `B ~rot cond

  | `UXTAH, [|dest; src; shift; rot; cond; _|] ->
    extend ~dest ~src:shift ~src2:src Unsigned `H ~rot cond

  | `SXTAB, [|dest; src; shift; rot; cond; _|] ->
    extend ~dest ~src:shift ~src2:src Signed `B ~rot cond

  | `SXTAH, [|dest; src; shift; rot; cond; _|] ->
    extend ~dest ~src:shift ~src2:src Signed `H ~rot cond

  (* extracts *)
  | `UBFX, [|dest; src; lsb; widthminus1; cond; _|] ->
    bit_extract ~dest ~src Unsigned ~lsb ~widthminus1 cond

  | `SBFX, [|dest; src; lsb; widthminus1; cond; _|] ->
    bit_extract ~dest ~src Signed   ~lsb ~widthminus1 cond


  (* bit field *)
  | `BFI, [|dest; _unknown; src; _bmask; cond; _|] ->
    bit_field_insert ~dest ~src mem cond

  | `BFC, [|dest; _unknown; _bmask; cond; _|] ->
    bit_field_insert ~dest ~src:(Imm (word 0)) mem cond

  (* bit reverse *)
  | `RBIT, [|dest; src; cond; _|] ->
    let dest = assert_reg _here_ dest in
    let v = Env.new_tmp "v"  in
    let r = Env.new_tmp "r"  in
    let s = Env.new_tmp "s"  in
    let open Exp in
    let open Stmt in
    exec [
      move v (exp_of_op src lsr int32 1);
      move r (exp_of_op src);
      move s (int32 31);
      While (Exp.(var v <> int32 0), [
          Move (r, Var r lsl int32 1);
          Move (r, Var r lor (Var v land int32 1));
          Move (s, Var s - int32 1);
        ]);
      Move (Env.of_reg dest, Var r lsl Var s);
    ] cond

  (* Swap bytes *)
  | `SWPB, [|Reg dest; Reg src1; Reg src2; cond; _|] ->
    let temp = Var.create ~tmp:true "x" reg8_t in
    let dest = Env.of_reg dest in
    let src1 = Env.of_reg src1 |> Exp.var in
    let src2 = Env.of_reg src2 |> Exp.var in
    let mem = Env.new_mem "mem" in
    exec [
      assn temp Exp.(load (var mem) src2 LittleEndian `r8);
      Stmt.move mem
        Exp.(store (var mem) src2 (extract 7 0 src1) LittleEndian `r8);
      assn dest Exp.(cast Cast.unsigned 32 (var temp));
    ] cond

  (* Pack half *)
  | `PKHTB, [|Reg dest; src1; src2; shift; cond; _|] ->
    (* shift is always asr *)
    let shifted, _ =
      Shift.lift_c ~src:(exp_of_op src2) `ASR
        ~shift:(exp_of_op shift) reg32_t in
    exec [
      assn (Env.of_reg dest)
        Exp.(extract 31 16 (exp_of_op src1) ^
             extract 15  0  shifted)
    ] cond
  (* reverses *)
  | `REV, [|Reg dest; src; cond; _|] ->
    let s = exp_of_op src in
    let i24 = int32 24 in
    let i8 = int32 8 in
    let umask = int32 0xff0000 in
    let lmask = int32 0xff00 in
    let rev =
      let open Exp in
      s              lsl i24 lor
      s              lsr i24 lor
      (s land umask) lsr i8  lor
      (s land lmask) lsl i8
    in
    exec [assn (Env.of_reg dest) rev] cond
  | `REV16, [|Reg dest; src; cond; _|] ->
    let s = exp_of_op src in
    let i16 = int32 16 in
    let rev = Exp.(s lsl i16 lor s lsr i16) in
    exec [assn (Env.of_reg dest) rev] cond
  | `CLZ, [|Reg dest; src; cond; _|] ->
    let shift = Env.new_tmp "shift" in
    let accum = Env.new_tmp "accum" in
    let open Exp in
    exec [
      Stmt.move shift (exp_of_op src);
      Stmt.move accum (int32 32);
      Stmt.While (var shift <> int32 0, [
          Stmt.move shift (var shift lsr int32 1);
          Stmt.move accum (var accum - int32 1);
        ]);
      Stmt.move (Env.of_reg dest) (var accum);

    ] cond
  | insn,ops ->
    fail _here_ "ops %s doesn't match bits insn %s"
      (string_of_ops ops) (Arm.Insn.to_string (insn :> insn))




let lift_mult ops insn =
  let open Mul in
  match insn,ops with
  | `MUL, [|Reg dest; src1; src2; cond; _rflag; wflag|] ->
    let flags = Flags.set_nzf Exp.(var (Env.of_reg dest)) reg32_t in
    exec [
      assn (Env.of_reg dest) Exp.(exp_of_op src1 * exp_of_op src2)
    ] ~flags ~wflag cond

  | `MLA, [|Reg dest; src1; src2; addend; cond; _rflag; wflag|] ->
    let flags = Flags.set_nzf Exp.(var Exp.(Env.of_reg dest)) reg32_t in
    exec [
      assn (Env.of_reg dest)
        Exp.(exp_of_op addend + exp_of_op src1 * exp_of_op src2)
    ] ~flags ~wflag cond

  | `MLS, [|Reg dest; src1; src2; addend; cond; _|] ->
    exec [
      Stmt.move (Env.of_reg dest)
        Exp.(exp_of_op addend - exp_of_op src1 * exp_of_op src2)
    ] cond

  | `UMULL, [|lodest; hidest; src1; src2; cond; _rflag; wflag|] ->
    lift_mull ~lodest ~hidest ~src1 ~src2 Unsigned ~wflag cond

  | `SMULL, [|lodest; hidest; src1; src2; cond; _rflag; wflag|] ->
    lift_mull ~lodest ~hidest ~src1 ~src2 Signed ~wflag cond

  | `UMLAL, [|lodest; hidest; src1; src2;
              _loadd; _hiadd; cond; _rflag; wflag|] ->
    lift_mull ~lodest ~hidest ~src1 ~src2 Unsigned ~addend:true ~wflag cond

  | `SMLAL, [|lodest; hidest; src1; src2;
              _loadd; _hiadd; cond; _rflag; wflag|] ->
    lift_mull ~lodest ~hidest ~src1 ~src2 Signed ~addend:true ~wflag cond

  (* signed 16bit mul plus a 32bit bit accum, Q *)
  | `SMLABB, [|dest; src1; src2; accum; cond; _wflag|] ->
    lift_smul ~dest ~src1 ~src2 ~accum ~q:true BB cond

  (* signed 16bit mul *)
  | `SMULBB, [|dest; src1; src2; cond; _wflag|] ->
    lift_smul ~dest ~src1 ~src2 BB cond

  (* two signed 16bit muls plus 32bit accum and optional xchg, Q*)
  | `SMLAD, [|dest; src1; src2; accum; cond; _wflag|] ->
    lift_smul ~dest ~src1 ~src2 ~accum ~q:true D cond

  (* two signed 16bit muls and optional xchg, Q *)
  | `SMUAD, [|dest; src1; src2; cond; _wflag|] ->
    lift_smul ~dest ~src1 ~src2 ~q:true D cond

  (* signed 16bit times signed 32bit added to 32bit accum, Q *)
  | `SMLAWB, [|dest; src1; src2; accum; cond; _wflag|] ->
    lift_smul ~dest ~src1 ~src2 ~accum ~q:true WB cond

  (* signed 16bit mul *)
  | `SMULTB, [|dest; src1; src2; cond; _wflag|] ->
    lift_smul ~dest ~src1 ~src2 TB cond

  (* signed 16bit mul plus 64bit accum *)
  | `SMLALBT, [|dest; hidest; src1; src2; cond; _wflag|] ->
    lift_smul ~dest ~hidest ~src1 ~src2 ~accum:dest ~hiaccum:hidest BT cond

  | insn,ops ->
    fail _here_ "ops %s doesn't match mult insn %s"
      (string_of_ops ops) (Arm.Insn.to_string (insn :> insn))


let lift_mem_multi ops insn =
  match insn, Array.to_list ops with
  | `STMDA, base :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DA NoUpdate St in
    exec insns cond

  | `STMDA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DA Update St in
    exec insns cond

  | `LDMIB, base :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m dest_list base IB NoUpdate Ld in
    exec insns cond

  | `LDMIB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m dest_list base IB Update Ld in
    exec insns cond

  | `STMIB, base :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m dest_list base IB NoUpdate St in
    exec insns cond

  | `STMIB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m dest_list base IB Update St in
    exec insns cond

  | `LDMDB, base :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DB NoUpdate Ld in
    exec insns cond

  | `LDMDB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DB Update Ld in
    exec insns cond

  | `STMDB, base :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DB NoUpdate St in
    exec insns cond

  | `STMDB_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DB Update St in
    exec insns cond

  | `LDMIA, base ::  cond ::  _wr_flag ::  dest_list  ->
    let insns = Mem_shift.lift_m dest_list base IA NoUpdate Ld in
    exec insns cond

  | `LDMIA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list  ->
    let insns = Mem_shift.lift_m dest_list base IA Update Ld in
    exec insns cond

  | `STMIA, base :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m dest_list base IA NoUpdate St in
    exec insns cond

  | `STMIA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m dest_list base IA Update St in
    exec insns cond

  | `LDMDA, base :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DA NoUpdate Ld in
    exec insns cond

  | `LDMDA_UPD, base :: _unknown :: cond :: _wr_flag :: dest_list ->
    let insns = Mem_shift.lift_m (List.rev dest_list)
        base DA Update Ld in
    exec insns cond

  | _ ->
    fail _here_ "ops %s doesn't match multi arg insn %s"
      (string_of_ops ops) (Arm.Insn.to_string (insn :> insn))


let lift_mem ops insn =
  let open Mem in

  match insn, ops with
  | `STRD, [|dest1; dest2; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~dest2 ~base ~offset
        Offset Unsigned D St in
    exec insns cond

  | `LDRD, [|dest1; dest2; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~dest2 ~base ~offset
        Offset Unsigned D Ld in
    exec insns cond

  | `STRD_POST, [|dest1; dest2; base; _unknown; reg_off; Imm imm_off;
                  cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~dest2 ~base ~offset
        PostIndex Unsigned D St in
    exec insns cond

  | `LDRD_POST, [|dest1; dest2; base; _unknown; reg_off; Imm imm_off;
                  cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~dest2 ~base ~offset
        PostIndex Unsigned D Ld
    in
    exec insns cond

  | `STRD_PRE, [|_unknown; dest1; dest2; base; reg_off; Imm imm_off;
                 cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~dest2 ~base ~offset
        PreIndex Unsigned D St
    in
    exec insns cond

  | `LDRD_PRE, [|dest1; dest2; _unknown; base; reg_off; Imm imm_off;
                 cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~dest2 ~base ~offset
        PreIndex Unsigned D Ld
    in
    exec insns cond

  | `STRH, [|dest1; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset
        Offset Unsigned H St
    in
    exec insns cond

  | `LDRH, [|dest1; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset
        Offset Unsigned H Ld
    in
    exec insns cond

  | `STRH_PRE, [|_unknown; dest1; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset
        PreIndex Unsigned H St
    in
    exec insns cond

  | `LDRH_PRE, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset
        PreIndex Unsigned H Ld
    in
    exec insns cond

  | `STRH_POST, [|_unknown; dest1; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset
        PostIndex Unsigned H St
    in
    exec insns cond

  (* Unlike the convention of all other load and store instructions, for some
   * instructions the sign bit is set in the immediate when the operand
   * is POSITIVE. Insructions that are affected by this are marked with
   * "POS_SIGN_BIT"
   **)
  (* POS_SIGN_BIT *)
  | `STRHTr, [|_unknown; dest1; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_pos reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset
        PostIndex Unsigned H St
    in
    exec insns cond

  | `LDRH_POST, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset
        PostIndex Unsigned H Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | `LDRHTr, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_pos reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Unsigned H Ld
    in
    exec insns cond

  | `LDRSH, [|dest1; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset Offset Signed H Ld
    in
    exec insns cond

  | `LDRSH_PRE, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PreIndex Signed H Ld
    in
    exec insns cond

  | `LDRSH_POST, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Signed H Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | `LDRSHTr, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_pos reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Signed H Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | `LDRSHTi, [|dest1; _unknown; base; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_pos (Op.Reg `nil) imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Signed H Ld
    in
    exec insns cond

  | `LDRSB, [|dest1; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset Offset Signed B Ld
    in
    exec insns cond

  | `LDRSB_PRE, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PreIndex Signed B Ld
    in
    exec insns cond

  | `LDRSB_POST, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_neg reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Signed B Ld
    in
    exec insns cond

  (* POS_SIGN_BIT *)
  | `LDRSBTr, [|dest1; _unknown; base; reg_off; Imm imm_off; cond; _|] ->
    let offset = Mem_shift.mem_offset_reg_or_imm_pos reg_off imm_off in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Signed B Ld
    in
    exec insns cond

  | `STRi12, [|dest1; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset Offset Unsigned W St
    in
    exec insns cond

  | `LDRi12, [|dest1; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset Offset Unsigned W Ld
    in
    exec insns cond

  | `STRBi12, [|dest1; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset Offset Unsigned B St
    in
    exec insns cond

  | `LDRBi12, [|dest1; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset Offset Unsigned B Ld
    in
    exec insns cond

  | `STRrs, [|dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        Offset Unsigned W St
    in
    exec insns cond

  | `LDRrs, [|dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        Offset Unsigned W Ld
    in
    exec insns cond

  | `STRBrs, [|dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        Offset Unsigned B St
    in
    exec insns cond

  | `LDRBrs, [|dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        Offset Unsigned B Ld
    in
    exec insns cond

  | `STR_POST_IMM, [|_unknown; dest1; base; _invalid; Imm offset; cond; _|] ->
    let offset =
      Mem_shift.repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Unsigned W St
    in
    exec insns cond

  | `LDR_POST_IMM, [|dest1; _unknown; base; _invalid; Imm offset; cond; _|] ->
    let offset =
      Mem_shift.repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Unsigned W Ld
    in
    exec insns cond

  | `STRB_POST_IMM,  [|_unknown; dest1; base; _invalid; Imm offset; cond; _|]
  | `STRBT_POST_IMM, [|_unknown; dest1; base; _invalid; Imm offset; cond; _|]
    ->
    let offset =
      Mem_shift.repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Unsigned B St
    in
    exec insns cond

  | `LDRB_POST_IMM,  [|dest1; _unknown; base; _invalid; Imm offset; cond; _|]
  | `LDRBT_POST_IMM, [|dest1; _unknown; base; _invalid; Imm offset; cond; _|]
    ->
    let offset =
      Mem_shift.repair_imm offset ~sign_mask:0x1000 ~imm_mask:0xfff `NEG
    in
    let insns =
      Mem_shift.lift_r_exp ~dest1 ~base ~offset PostIndex Unsigned B Ld
    in
    exec insns cond

  | `STR_POST_REG,  [|_unknown; dest1; base; offset; shift; cond; _|]
  | `STRT_POST_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PostIndex Unsigned W St
    in
    exec insns cond

  | `LDR_POST_REG,  [|dest1; _unknown; base; offset; shift; cond; _|]
  | `LDRT_POST_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PostIndex Unsigned W Ld
    in
    exec insns cond

  | `STRB_POST_REG,  [|_unknown; dest1; base; offset; shift; cond; _|]
  | `STRBT_POST_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PostIndex Unsigned B St
    in
    exec insns cond

  | `LDRB_POST_REG,  [|dest1; _unknown; base; offset; shift; cond; _|]
  | `LDRBT_POST_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PostIndex Unsigned B Ld
    in
    exec insns cond

  | `STR_PRE_IMM, [|_unknown; dest1; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset PreIndex Unsigned W St
    in
    exec insns cond

  | `LDR_PRE_IMM, [|dest1; _unknown; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset PreIndex Unsigned W Ld
    in
    exec insns cond

  | `STRB_PRE_IMM, [|_unknown; dest1; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset PreIndex Unsigned B St
    in
    exec insns cond

  | `LDRB_PRE_IMM, [|dest1; _unknown; base; offset; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset PreIndex Unsigned B Ld
    in
    exec insns cond

  | `STR_PRE_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PreIndex Unsigned W St
    in
    exec insns cond

  | `LDR_PRE_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PreIndex Unsigned W Ld
    in
    exec insns cond

  | `STRB_PRE_REG, [|_unknown; dest1; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PreIndex Unsigned B St
    in
    exec insns cond

  | `LDRB_PRE_REG, [|dest1; _unknown; base; offset; shift; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset ~shift
        PreIndex Unsigned B Ld
    in
    exec insns cond

  (* Exclusive access, we may later want to do something special to these *)

  | `LDREX, [|dest1; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset:(Imm (word 0))
        Offset Unsigned W Ld
    in
    exec insns cond

  | `LDREXB, [|dest1; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset:(Imm (word 0))
        Offset Unsigned B Ld
    in
    exec insns cond

  | `LDREXH, [|dest1; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1 ~base ~offset:(Imm (word 0))
        Offset Unsigned H Ld
    in
    exec insns cond

  (* multidest is one of the multireg combinations *)
  | `LDREXD, [|multidest; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1:multidest ~base ~offset:(Imm (word 0))
        Offset Unsigned D Ld
    in
    exec insns cond

  | `STREX, [|Reg dest1; src1; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1:src1 ~base ~offset:(Imm (word 0))
        Offset Unsigned W St in
    let result = [Stmt.move (Env.of_reg dest1) (int32 0)] in
    exec (insns @ result) cond

  | `STREXB, [|Reg dest1; src1; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1:src1 ~base ~offset:(Imm (word 0))
        Offset Unsigned B St
    in
    let result = [Stmt.move (Env.of_reg dest1) (int32 0)] in
    exec (insns @ result) cond

  | `STREXH, [|Reg dest1; src1; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1:src1 ~base ~offset:(Imm (word 0))
        Offset Unsigned H St
    in
    let result = [Stmt.move (Env.of_reg dest1) (int32 0)] in
    exec (insns @ result) cond

  (* multisrc is one of the multireg combinations *)
  | `STREXD, [|Reg dest1; multisrc; base; cond; _|] ->
    let insns =
      Mem_shift.lift_r_op ~dest1:multisrc ~base ~offset:(Imm (word 0))
        Offset Unsigned D St
    in
    let result = [Stmt.move (Env.of_reg dest1) (int32 0)] in
    exec (insns @ result) cond
  | #Insn.mem_multi as insn, ops -> lift_mem_multi ops insn
  | insn,ops ->
    fail _here_ "ops %s doesn't match mem access insn %s"
      (string_of_ops ops) (Arm.Insn.to_string (insn :> insn))


(** Branching instructions *)

let lift_branch mem ops insn =
  let addr = Memory.min_addr mem in
  match insn, ops with

  | `Bcc, [|offset; cond; _|] ->
    Branch.lift offset ~cond addr

  | `BL, [|offset; cond; _|]
  | `BL_pred, [|offset; cond; _|] ->
    Branch.lift offset ~cond ~link:true addr

  | `BX_RET, [|cond; _|] ->
    Branch.lift (Reg `LR) ~cond ~x:true addr

  | `BX, [|target|] ->
    Branch.lift target ~x:true addr

  | `BX_pred, [|target; cond; _|] ->
    Branch.lift target ~cond ~x:true addr

  | `BLX, [|target|] ->
    Branch.lift target ~link:true ~x:true addr

  | `BLX_pred, [|target; cond; _|] ->
    Branch.lift target ~cond ~link:true ~x:true addr

  | `BLXi, [|offset|] ->
    Branch.lift offset ~link:true ~x:true addr

  | insn,ops ->
    fail _here_ "ops %s doesn't match branch insn %s"
      (string_of_ops ops) (Arm.Insn.to_string (insn :> insn))


let lift_special ops insn =
  match insn, ops with
  (* supervisor call *)
  | `SVC, [|Imm word; cond; _|] ->
    exec [Stmt.special (Format.asprintf "svc %a" Word.pp word)] cond

  | `MRS, [|Reg dest; cond; _|] ->
    let get_bits flag src lsb =
      Exp.(src lor (cast Cast.unsigned 32 (var flag) lsl int32 lsb)) in
    let d = Env.of_reg dest in
    let vd = Exp.var d in
    exec [
      Stmt.move d (int32 0);
      Stmt.move d (get_bits Env.nf vd 31);
      Stmt.move d (get_bits Env.zf vd 30);
      Stmt.move d (get_bits Env.cf vd 29);
      Stmt.move d (get_bits Env.vf vd 28);
      Stmt.move d (get_bits Env.qf vd 27);
      Stmt.move d (get_bits Env.ge.(3) vd 19);
      Stmt.move d (get_bits Env.ge.(2) vd 18);
      Stmt.move d (get_bits Env.ge.(1) vd 17);
      Stmt.move d (get_bits Env.ge.(0) vd 16);
    ] cond

  (* Move to special from register
   * For MSR an immediate with bit x set means:
   * bit 0 is CPSR_c (is not valid in ARMv7)
   * bit 1 is CPSR_x (is not valid in ARMv7)
   * bit 2 is APSR_g
   * bit 3 is APSR_nzcvq
   **)
  | `MSR, [|Imm imm; Reg src; cond; _|] ->
    let src = Exp.var (Env.of_reg src) in
    let (:=) flag bit = Stmt.move flag (Exp.extract bit bit src) in
    let s1 =
      if Word.(Int_exn.(imm land word 0x8) = word 0x8) then [
        Env.nf := 31;
        Env.zf := 30;
        Env.cf := 29;
        Env.vf := 28;
        Env.qf := 27;
      ] else [] in
    let s2 =
      if Word.(Int_exn.(imm land word 0x4) = word 0x4) then [
        Env.ge.(3) := 19;
        Env.ge.(2) := 18;
        Env.ge.(1) := 17;
        Env.ge.(0) := 16;
      ] else [] in
    exec (s1 @ s2) cond
  (* All of these are nops in User mode *)
  | `CPS2p, _ | `DMB, _ | `DSB, _ | `HINT, _ | `PLDi12, _ -> []

  | insn,ops ->
    fail _here_ "ops %s doesn't match special insn %s"
      (string_of_ops ops) (Arm.Insn.to_string (insn :> insn))

let arm_ops_exn ops () =
  Array.map (ops) ~f:(fun op ->
      Option.value_exn
        ~here:_here_
        ~error:(Error.create "unsupported operand" op Basic.Op.sexp_of_t )
        (Arm.Op.create op))

let arm_ops ops = try_with (arm_ops_exn ops)


let insn_exn mem insn =
  let open Arm.Insn in
  let name = Basic.Insn.name insn in
  Memory.(Addr.Int.(!$(max_addr mem) - !$(min_addr mem)))
  >>= Word.to_int >>= fun s -> Size.of_int ((s+1) * 8) >>= fun size ->
  Memory.get ~scale:(size ) mem >>| fun word ->
  match Arm.Insn.create insn with
  | None -> [Stmt.special (sprintf "unsupported: %s" name)]
  | Some arm_insn -> match arm_ops (Basic.Insn.ops insn) with
    | Error err -> [Stmt.special (Error.to_string_hum err)]
    | Ok ops -> match arm_insn with
      | #move as op -> lift_move word ops op
      | #bits as op -> lift_bits word ops op
      | #mult as op -> lift_mult ops op
      | #mem  as op -> lift_mem  ops op
      | #branch as op -> lift_branch mem ops op
      | #special as op -> lift_special ops op

let insn mem insn =
  try insn_exn mem insn with
  | Lifting_failed msg -> errorf "%s:%s" (Basic.Insn.name insn) msg
  | exn -> of_exn exn
