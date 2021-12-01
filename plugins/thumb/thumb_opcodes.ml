open Core_kernel
open Bap.Std

(* all the `mov` series, registers marked with `e` means extended *)
type opmov = [
  |  `tADC
  | `tADDi3
  | `tADDi8
  | `tADDrSPi
  | `tADDrr
  | `tADDspi
  | `tASRri
  | `tCMPi8
  | `tCMPr
  | `tLSLri
  | `tLSRri
  | `tMOVSr
  | `tMOVi8
  | `tORR
  | `tSUBi3
  | `tSUBi8
  | `tSUBrr
  | `tSUBspi
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
  | opmem_multi
  | `t2LDRpci
  | `tLDRBi
  | `tLDRBr
  | `tLDRHi
  | `tLDRHr
  | `tLDRSB
  | `tLDRSH
  | `tLDRi
  | `tLDRpci
  | `tLDRr
  | `tLDRspi
  | `tSTRBi
  | `tSTRBr
  | `tSTRHi
  | `tSTRHr
  | `tSTRi
  | `tSTRr
  | `tSTRspi
] [@@deriving bin_io, compare, sexp, enumerate]

type opbranch = [ `tB | `tBL | `tBLXi | `tBLXr | `tBX | `tBcc | `tCBNZ | `tCBZ ]
[@@deriving bin_io, compare, sexp, enumerate]

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
