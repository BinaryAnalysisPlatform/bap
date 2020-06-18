open Core_kernel
open Regular.Std
open Bap.Std
(*
1. Also has Wn. Do I need to define?
2. Same name but extended
*)

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

    | `WZR (* 32-bit zero register *)
    | `XZR (* 64-bit zero register *)
    | `WSP (* 32-bit stack pointer *)
    | `SP (* 64-bit stack pointer *)
    | `PC (* program pointer *)
    | `ELR (* exception link register *)
    | `SPSR (* saved processor state register *)
]

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

type op = [
    | `Reg of registers_t
    | `Imm of word
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
    | `LDR
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
type insn = [
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
