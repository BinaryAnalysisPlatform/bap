open Core_kernel
open Regular.Std
open Bap.Std

module Basic = Disasm_expert.Basic

exception Lifting_failed of string

type cond = [
  | `EQ
  | `NE
  | `CS
  | `CC
  | `MI
  | `PL
  | `VS
  | `VC
  | `HI
  | `LS
  | `GE
  | `LT
  | `GT
  | `LE
  | `AL
] [@@deriving bin_io, compare, sexp, enumerate]

type nil_reg = [ `Nil ]
[@@deriving bin_io, compare, sexp, enumerate]

(** General purpose registers  *)
type gpr_reg = [
  | `R0
  | `R1
  | `R2
  | `R3
  | `R4
  | `R5
  | `R6
  | `R7
  | `R8
  | `R9
  | `R10
  | `R11
  | `R12
  | `LR
  | `PC
  | `SP
] [@@deriving bin_io, compare, sexp, enumerate]

type gpr_or_nil = [nil_reg | gpr_reg]
[@@deriving bin_io, compare, sexp, enumerate]

(** conditition code registers  *)
type ccr_reg = [
  | `CPSR
  | `SPSR
  | `ITSTATE
] [@@deriving bin_io, compare, sexp, enumerate]

type ccr_or_nil = [nil_reg | ccr_reg ]
[@@deriving bin_io, compare, sexp, enumerate]

type non_nil_reg = [gpr_reg | ccr_reg]
[@@deriving bin_io, compare, sexp, enumerate]

type reg = [nil_reg | non_nil_reg]
[@@deriving bin_io, compare, sexp, enumerate]

type op = [
  | `Reg of reg
  | `Imm of word
] [@@deriving bin_io, compare, sexp]

type move_insn = [
  | `ADCri
  | `ADCrr
  | `ADCrsi
  | `ADCrsr
  | `ADDri
  | `ADDrr
  | `ADDrsi
  | `ADDrsr
  | `ANDri
  | `ANDrr
  | `ANDrsi
  | `ANDrsr
  | `BICri
  | `BICrr
  | `BICrsi
  | `BICrsr
  | `CMNri
  | `CMNzrr
  | `CMNzrsi
  | `CMNzrsr
  | `CMPri
  | `CMPrr
  | `CMPrsi
  | `CMPrsr
  | `EORri
  | `EORrr
  | `EORrsi
  | `EORrsr
  | `MOVTi16
  | `MOVi
  | `MOVi16
  | `MOVr
  | `MOVsi
  | `MOVsr
  | `MOVPCLR
  | `MVNi
  | `MVNr
  | `MVNsi
  | `MVNsr
  | `ORRri
  | `ORRrr
  | `ORRrsi
  | `ORRrsr
  | `RSBri
  | `RSBrr
  | `RSBrsi
  | `RSBrsr
  | `RSCri
  | `RSCrr
  | `RSCrsi
  | `RSCrsr
  | `SBCri
  | `SBCrr
  | `SBCrsi
  | `SBCrsr
  | `SUBri
  | `SUBrr
  | `SUBrsi
  | `SUBrsr
  | `TEQri
  | `TEQrr
  | `TEQrsi
  | `TEQrsr
  | `TSTri
  | `TSTrr
  | `TSTrsi
  | `TSTrsr
] [@@deriving bin_io, compare, sexp, enumerate]

type bits_insn = [
  | `BFC
  | `BFI
  | `PKHTB
  | `RBIT
  | `SBFX
  | `SWPB
  | `SXTAB
  | `SXTAH
  | `SXTB
  | `SXTH
  | `UBFX
  | `UXTAB
  | `UXTAH
  | `UXTB
  | `UXTH
  | `REV
  | `REV16
  | `CLZ
] [@@deriving bin_io, compare, sexp, enumerate]

type mult_insn = [
  | `MLA
  | `MLS
  | `MUL
  | `SMLABB
  | `SMLAD
  | `SMLAL
  | `SMLALBT
  | `SMLAWB
  | `SMUAD
  | `SMULBB
  | `SMULL
  | `SMULTB
  | `UMLAL
  | `UMULL
] [@@deriving bin_io, compare, sexp, enumerate]


type mem_multi_insn = [
  | `LDMDA
  | `LDMDA_UPD
  | `LDMDB
  | `LDMDB_UPD
  | `LDMIA
  | `LDMIA_UPD
  | `LDMIB
  | `LDMIB_UPD
  | `STMDA
  | `STMDA_UPD
  | `STMDB
  | `STMDB_UPD
  | `STMIA
  | `STMIA_UPD
  | `STMIB
  | `STMIB_UPD
] [@@deriving bin_io, compare, sexp, enumerate]


type mem_insn = [
  | mem_multi_insn
  | `LDRBT_POST_IMM
  | `LDRBT_POST_REG
  | `LDRB_POST_IMM
  | `LDRB_POST_REG
  | `LDRB_PRE_IMM
  | `LDRB_PRE_REG
  | `LDRBi12
  | `LDRBrs
  | `LDRD
  | `LDRD_POST
  | `LDRD_PRE
  | `LDREX
  | `LDREXB
  | `LDREXD
  | `LDREXH
  | `LDRH
  | `LDRHTr
  | `LDRH_POST
  | `LDRH_PRE
  | `LDRSB
  | `LDRSBTr
  | `LDRSB_POST
  | `LDRSB_PRE
  | `LDRSH
  | `LDRSHTi
  | `LDRSHTr
  | `LDRSH_POST
  | `LDRSH_PRE
  | `LDRT_POST_REG
  | `LDR_POST_IMM
  | `LDR_POST_REG
  | `LDR_PRE_IMM
  | `LDR_PRE_REG
  | `LDRi12
  | `LDRrs
  | `STRBT_POST_IMM
  | `STRBT_POST_REG
  | `STRB_POST_IMM
  | `STRB_POST_REG
  | `STRB_PRE_IMM
  | `STRB_PRE_REG
  | `STRBi12
  | `STRBrs
  | `STRD
  | `STRD_POST
  | `STRD_PRE
  | `STREX
  | `STREXB
  | `STREXD
  | `STREXH
  | `STRH
  | `STRHTr
  | `STRH_POST
  | `STRH_PRE
  | `STRT_POST_REG
  | `STR_POST_IMM
  | `STR_POST_REG
  | `STR_PRE_IMM
  | `STR_PRE_REG
  | `STRi12
  | `STRrs
] [@@deriving bin_io, compare, sexp, enumerate]

type branch_insn = [
  | `BL
  | `BLX
  | `BLX_pred
  | `BLXi
  | `BL_pred
  | `BX
  | `BX_RET
  | `BX_pred
  | `Bcc
] [@@deriving bin_io, compare, sexp, enumerate]

type special_insn = [
  | `CPS2p
  | `DMB
  | `DSB
  | `HINT
  | `MRS
  | `MSR
  | `PLDi12
  | `SVC
] [@@deriving bin_io, compare, sexp, enumerate]

type insn = [
  | move_insn
  | bits_insn
  | mult_insn
  | mem_insn
  | branch_insn
  | special_insn
] [@@deriving bin_io, compare, sexp, enumerate]

(** Memory access operations *)

(** Types for single-register memory access *)
type mode_r = Offset | PreIndex | PostIndex
type sign = Signed | Unsigned
type operation = Ld | St
type size = B | H | W | D
[@@deriving compare]

(** Types for multiple-register memory access *)
type mode_m = IA | IB | DA | DB
type update_m = Update | NoUpdate

(** Types for data movement operations  *)
type arth = [`ADD | `ADC | `SBC | `RSC | `SUB | `RSB ]
type move = [`AND | `BIC | `EOR | `MOV | `MVN | `ORR ]
type data_oper = [ arth | move]

type repair = [`POS | `NEG] [@@deriving compare]

(** shift types *)
type shift = [`ASR | `LSL | `LSR | `ROR | `RRX]


type smul_size = BB | BT | TB | TT | D | DX | WB | WT
