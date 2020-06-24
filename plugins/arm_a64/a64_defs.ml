open Core_kernel
open Regular.Std
open Bap.Std

module QWord = Bitvec.M64
module Word = Bitvec.M32
module Byte = Bitvec.M8

type registers_t = [
  (* parameter and result registers *)
  | `X0 | `X1 | `X2 | `X3 | `X4 | `X5 | `X6 | `X7 

  (* caller saved temporary registers *)
  | `X9 | `X10 | `X11 | `X12 | `X13 | `X14 | `X15 

  (* callee saved registers *) 
  | `X19 | `X20 | `X21 | `X22 | `X23 | `X24 | `X25 | `X26 | `X27 | `X28

  | `X8
  | `X16 | `X17 | `X18
  | `X29 | `X30

  (* parameter and result registers *)
  | `W0 | `W1 | `W2 | `W3 | `W4 | `W5 | `W6 | `W7 

  (* caller saved temporary registers *)
  | `W9 | `W10 | `W11 | `W12 | `W13 | `W14 | `W15 

  (* callee saved registers *) 
  | `W19 | `W20 | `W21 | `W22 | `W23 | `W24 | `W25 | `W26 | `W27 | `W28

  | `W8
  | `W16 | `W17 | `W18
  | `W29 | `W30

  | `WZR (* 32-bit zero register *)
  | `XZR (* 64-bit zero register *)
  | `WSP (* 32-bit stack pointer *)
  | `SP (* 64-bit stack pointer *)
  | `PC (* program pointer *)
  | `ELR (* exception link register *)
  | `SPSR (* saved processor state register *)
]

type sign_t = Signed | Unsigned
type operation_t = LD | ST
type bv_size_t = 
  | B (* byte, 8 bits *) 
  | H (* halfword, 16 bits *)
  | W (* word, 32 bits *)
  | X (* doubleword, 64 bits *)

type extend_t =
  | SXTB
  | SXTH
  | SXTW
  | SXTX
  | UXTB
  | UXTH
  | UXTW
  | UXTX
  | LSL

type conditon_t = [
  | `EQ
  | `NE
  | `CS | `HS
  | `CC | `LO
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
  | `NV
]

type operand_t = [
  | `Reg of registers_t
  | `Imm of Bitvec.t
]

type branch_insn_t = [
  (* conditional branch TODO*)
  | `CBNZ
  (* unconditional branch (immediate) *)
  | `B
  | `BL
  (* unconditional branch (register) *)
  | `BLR
  | `BR
  | `RET
]

type mem_access_insn_t = [
  (* load-store single register TODO *)
  | `LDRi_pre_64
  | `LDRi_post_64
  | `LDRi_unsigned_offset_64
  | `LDRi_pre_32
  | `LDRi_post_32
  | `LDRi_unsigned_offset_32

  | `LDRr_UXTW_scaled_64
  | `LDRr_LSL_scaled_64
  | `LDRr_SXTW_scaled_64
  | `LDRr_SXTX_scaled_64
  | `LDRr_UXTW_unscaled_64
  | `LDRr_LSL_unscaled_64
  | `LDRr_SXTW_unscaled_64
  | `LDRr_SXTX_unscaled_64
  | `LDRr_UXTW_scaled_32
  | `LDRr_LSL_scaled_32
  | `LDRr_SXTW_scaled_32
  | `LDRr_SXTX_scaled_32
  | `LDRr_UXTW_unscaled_32
  | `LDRr_LSL_unscaled_32
  | `LDRr_SXTW_unscaled_32
  | `LDRr_SXTX_unscaled_32

  | `LDRl_signed_64
  | `LDRl_unsigned_64
  | `LDRl_signed_32
  | `LDRl_unsigned_32

  | `LDRB
  | `LDSB
  | `LDRH
  | `LDRSH
  | `LDRSW
  | `STR
  | `STRB
  | `STRH
  (* load-store single register (unscaled offset) TODO *)
  | `LDUR
  | `LDURB
  | `LDURSB
  | `LDURH
  | `LDURSH
  | `LDURSW
  | `STUR
  | `STURB
  | `STURH
  (* a lot of TODO *)    
]

type arithmetic_insn_t = [
  (* immediate *)
  (* shifted register *)
  (* extending register *)
  (* unshifted register *)
  | `todo
]

type logical_insn_t = [
  (* immediate *)
  (* shifted register *)
  | `todo
]
type move_insn_t = [
  (* immediate *)
  | `todo
]
type bitfield_insn_t = [
  (* operations *)
  | `todo
]
type shift_insn_t = [
  (* immediate *)
  | `todo
]
type sign_zero_extend_insn_t = [
  | `todo
]

type int_mult_div_insn_t = [
  (* multiply TODO *)
  | `MADD
  | `MSUB
  | `MNEG
  | `MUL
  | `SMADDL
  | `SMSUBL
  | `SMNEGL
  | `SMULL
  | `SMULH
  | `UMADDL
  | `UMSUBL
  | `UMNEGL
  | `UMULL
  | `UMULH
  (* divide TODO *)
  | `SDIV
  | `UDIV
]

(* floating points, SIMD *)
type simple_insn_t = [
  | branch_insn_t
  | mem_access_insn_t
  | arithmetic_insn_t
  | logical_insn_t
  | move_insn_t
  | bitfield_insn_t
  | shift_insn_t
  | sign_zero_extend_insn_t
  | int_mult_div_insn_t
]
