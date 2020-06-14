open Core_kernel
open Regular.Std
open Bap.Std

exception Lift_Error of string

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
(* all the ARM GRPs are still needed *)
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
(** all the `mov` series, registers marked with `e` means extended *)
type move_insn = [
  | `tMOVi8 (* Rd imm8 *)
  | `tMOVr (* Rde Rse *)
  | `tMOVSr (* Rd Rm affect CSPR *)
  | `tMVN (* Rd Rm *)
  | `tADC (* Rd Rn Rm *)
  | `tADDi3 (* Rd Rs imm *)
  | `tADDi8 (* Rd imm *)
  | `tADDrr (* Rd Rn Rm *)
  | `tADDhirr (* Rde Rse *)
  | `tADR (* Rd imm *)
  | `tADDrSPi (* Rd imm *)
  | `tADDspi (* imm *)
  | `tSBC (* Rd Rm *)
  | `tSUBi3 (* See the corresponding ADD insns. *)
  | `tSUBi8
  | `tSUBrr
  | `tSUBspi
  | `tMUL (* Rd Rn *)
] [@@deriving bin_io, compare, sexp, enumerate]

(** Rd [reglist] *)
type mem_multi_insn = [
  | `tSTMIA
  | `tLDMIA
] [@@deriving bin_io, compare, sexp, enumerate]


type mem_insn = [
  | mem_multi_insn
] [@@deriving bin_io, compare, sexp, enumerate]

type branch_insn = [
  | `tBL
] [@@deriving bin_io, compare, sexp, enumerate]

type insn = [
  | move_insn
  | mem_insn
  | branch_insn
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