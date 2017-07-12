open Core_kernel.Std
open Regular.Std
open Bap.Std

module Basic = Disasm_expert.Basic

exception Lifting_failed of string

type cond = [
  | `EQ
  | `NE
  | `GE
  | `LT
  | `LTU
  | `GEU
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
  | `R13
  | `R14
  | `R15
  | `R16
  | `R17
  | `R18
  | `R19
  | `R20
  | `R21
  | `R22
  | `R23
  | `R24
  | `R25
  | `R26
  | `R27
  | `R28
  | `R29
  | `R30
  | `R31
  | `PC
] [@@deriving bin_io, compare, sexp, enumerate]

type reg = [nil_reg | gpr_reg]
[@@deriving bin_io, compare, sexp, enumerate]

type op = [
  | `Reg of reg
  | `Imm of word
] [@@deriving bin_io, compare, sexp]

type move_insn = [
  | `ADD
  | `ADDI
  | `SLT
  | `SLTI
  | `SLTU
  | `SLTIU
  | `SLL
  | `SLLI
  | `SRL
  | `SRLI
  | `SRA
  | `SRAI
  | `AND
  | `ANDI
  | `OR
  | `ORI
  | `SUB
  | `XOR
  | `XORI
  | `LUI
  | `AUIPC
] [@@deriving bin_io, compare, sexp, enumerate]

type mem_insn = [
  | `LW
  | `LH
  | `LHU
  | `LB
  | `LBU
  | `SW
  | `SH
  | `SB
] [@@deriving bin_io, compare, sexp, enumerate]

type branch_insn = [
  | `BEQ
  | `BNE
  | `BGE
  | `BLT
  | `BLTU
  | `BGEU
  | `JAL
  | `JALR
  | `RET
] [@@deriving bin_io, compare, sexp, enumerate]

type csr_insn = [
  | `CSRRW
  | `CSRRS
  | `CSRRC
  | `CSRRWI
  | `CSRRSI
  | `CSSRCI
] [@@deriving bin_io, compare, sexp, enumerate]

type insn = [
  | move_insn
  | mem_insn
  | branch_insn
  | csr_insn
] [@@deriving bin_io, compare, sexp, enumerate]

(** Memory access operations *)

(** Types for single-register memory access *)
type sign = Signed | Unsigned
type operation = Ld | St
type size = B | H | W | D

(** Types for data movement operations  *)
type arth = [`ADD | `ADC | `SBC | `RSC | `SUB | `RSB ]
type move = [`AND | `BIC | `EOR | `MOV | `MVN | `ORR ]
type data_oper = [ arth | move]

type repair = [`POS | `NEG]

(** shift types *)
type shift = [`ASR | `LSL | `LSR | `ROR | `RRX]


type smul_size = BB | BT | TB | TT | D | DX | WB | WT
