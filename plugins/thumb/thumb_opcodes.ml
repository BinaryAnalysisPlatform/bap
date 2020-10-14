open Core_kernel
open Bap.Std

(* all the `mov` series, registers marked with `e` means extended *)
type opmov = [
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
  | `tAND
  | `tASRri
  | `tASRrr
  | `tBIC
  | `tCMNz
  | `tCMPi8
  | `tCMPr
  | `tCMPhir
  | `tEOR
  | `tLSLri
  | `tLSLrr
  | `tLSRri
  | `tLSRrr
  | `tORR
  | `tRSB (* NEG *)
  | `tREV
  | `tREV16
  | `tREVSH
  | `tROR
  | `tSBC (* Rd Rm *)
  | `tSUBi3 (* See the corresponding ADD insns. *)
  | `tSUBi8
  | `tSUBrr
  | `tSUBspi
  | `tTST
  | `tMUL (* Rd Rn *)
] [@@deriving bin_io, compare, sexp, enumerate]

type opbit = [
  | `tSXTB
  | `tSXTH
  | `tUXTB
  | `tUXTH
] [@@deriving bin_io, compare, sexp, enumerate]

(** Rd [reglist] *)
type opmem_multi = [
  | `tSTMIA
  | `tSTMIA_UPD
  | `tLDMIA
  | `tLDMIA_UPD
  | `tPOP
  | `tPUSH
] [@@deriving bin_io, compare, sexp, enumerate]


type opmem = [
  | `tLDRi
  | `tLDRr
  | `tLDRpci
  | `tLDRspi
  | `tLDRBi
  | `tLDRBr
  | `tLDRHi
  | `tLDRHr
  | `tLDRSB
  | `tLDRSH
  | `tSTRi
  | `tSTRr
  | `tSTRspi
  | `tSTRBi
  | `tSTRBr
  | `tSTRHi
  | `tSTRHr
  | opmem_multi
] [@@deriving bin_io, compare, sexp, enumerate]

type opbranch = [
  | `tBcc
  | `tB
  | `tBL
  | `tBLXi
  | `tBLXr
  | `tBX
] [@@deriving bin_io, compare, sexp, enumerate]

type opcode = [
  | opmov
  | opmem
  | opbranch
  | opbit
] [@@deriving bin_io, compare, sexp, enumerate]

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
