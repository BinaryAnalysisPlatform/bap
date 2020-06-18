open Bap_core_theory
open Base
open KB.Syntax

module Defs = A64_defs

module Env_registers = struct
    type bv64_t
    type bv32_t
    
    let bv64_sort : bv64_t Theory.Bitv.t Theory.Value.sort = Theory.Bitv.define 64
    let bv32_sort : bv32_t Theory.Bitv.t Theory.Value.sort = Theory.Bitv.define 32

    (* global variables: registers *)
    let x0 = Theory.Var.define bv64_sort "X0"
    let x1 = Theory.Var.define bv64_sort "X1"
    let x2 = Theory.Var.define bv64_sort "X2"
    let x3 = Theory.Var.define bv64_sort "X3"
    let x4 = Theory.Var.define bv64_sort "X4"
    let x5 = Theory.Var.define bv64_sort "X5"
    let x6 = Theory.Var.define bv64_sort "X6"
    let x7 = Theory.Var.define bv64_sort "X7"
    let x8 = Theory.Var.define bv64_sort "X8"
    let x9 = Theory.Var.define bv64_sort "X9"
    let x10 = Theory.Var.define bv64_sort "X10"
    let x11 = Theory.Var.define bv64_sort "X11"
    let x12 = Theory.Var.define bv64_sort "X12"
    let x13 = Theory.Var.define bv64_sort "X13"
    let x14 = Theory.Var.define bv64_sort "X14"
    let x15 = Theory.Var.define bv64_sort "X15"
    let x16 = Theory.Var.define bv64_sort "X16"
    let x17 = Theory.Var.define bv64_sort "X17"
    let x18 = Theory.Var.define bv64_sort "X18"
    let x19 = Theory.Var.define bv64_sort "X19"
    let x20 = Theory.Var.define bv64_sort "X20"
    let x21 = Theory.Var.define bv64_sort "X21"
    let x22 = Theory.Var.define bv64_sort "X22"
    let x23 = Theory.Var.define bv64_sort "X23"
    let x24 = Theory.Var.define bv64_sort "X24"
    let x25 = Theory.Var.define bv64_sort "X25"
    let x26 = Theory.Var.define bv64_sort "X26"
    let x27 = Theory.Var.define bv64_sort "X27"
    let x28 = Theory.Var.define bv64_sort "X28"
    let x29 = Theory.Var.define bv64_sort "X29"
    let x30 = Theory.Var.define bv64_sort "X30"
    let w0 = Theory.Var.define bv32_sort "W0"
    let w1 = Theory.Var.define bv32_sort "W1"
    let w2 = Theory.Var.define bv32_sort "W2"
    let w3 = Theory.Var.define bv32_sort "W3"
    let w4 = Theory.Var.define bv32_sort "W4"
    let w5 = Theory.Var.define bv32_sort "W5"
    let w6 = Theory.Var.define bv32_sort "W6"
    let w7 = Theory.Var.define bv32_sort "W7"
    let w8 = Theory.Var.define bv32_sort "W8"
    let w9 = Theory.Var.define bv32_sort "W9"
    let w10 = Theory.Var.define bv32_sort "W10"
    let w11 = Theory.Var.define bv32_sort "W11"
    let w12 = Theory.Var.define bv32_sort "W12"
    let w13 = Theory.Var.define bv32_sort "W13"
    let w14 = Theory.Var.define bv32_sort "W14"
    let w15 = Theory.Var.define bv32_sort "W15"
    let w16 = Theory.Var.define bv32_sort "W16"
    let w17 = Theory.Var.define bv32_sort "W17"
    let w18 = Theory.Var.define bv32_sort "W18"
    let w19 = Theory.Var.define bv32_sort "W19"
    let w20 = Theory.Var.define bv32_sort "W20"
    let w21 = Theory.Var.define bv32_sort "W21"
    let w22 = Theory.Var.define bv32_sort "W22"
    let w23 = Theory.Var.define bv32_sort "W23"
    let w24 = Theory.Var.define bv32_sort "W24"
    let w25 = Theory.Var.define bv32_sort "W25"
    let w26 = Theory.Var.define bv32_sort "W26"
    let w27 = Theory.Var.define bv32_sort "W27"
    let w28 = Theory.Var.define bv32_sort "W28"
    let w29 = Theory.Var.define bv32_sort "W29"
    let w30 = Theory.Var.define bv32_sort "W30"

    let wzr = Theory.Var.define bv32_sort "WZR"
    let xzr = Theory.Var.define bv64_sort "XZR"
    let wsp = Theory.Var.define bv32_sort "WSP"
    let sp = Theory.Var.define bv64_sort "SP"
    let pc = Theory.Var.define bv64_sort "PC"
    let elr = Theory.Var.define bv64_sort "ELR"
    let spsr = Theory.Var.define bv64_sort "SPSR"

    let load_register (r: Defs.registers_t) =
        match r with
        | `X0 | `W0 -> x0
        | `X1 | `W1 -> x1
        | `X2 | `W2 -> x2
        | `X3 | `W3 -> x3
        | `X4 | `W4 -> x4
        | `X5 | `W5 -> x5
        | `X6 | `W6 -> x6
        | `X7 | `W7 -> x7
        | `X8 | `W8 -> x8
        | `X9 | `W9 -> x9
        | `X10 | `W10 -> x10
        | `X11 | `W11 -> x11
        | `X12 | `W12 -> x12
        | `X13 | `W13 -> x13
        | `X14 | `W14 -> x14
        | `X15 | `W15 -> x15
        | `X16 | `W16 -> x16
        | `X17 | `W17 -> x17
        | `X18 | `W18 -> x18
        | `X19 | `W19 -> x19
        | `X20 | `W20 -> x20
        | `X21 | `W21 -> x21
        | `X22 | `W22 -> x22
        | `X23 | `W23 -> x23
        | `X24 | `W24 -> x24
        | `X25 | `W25 -> x25
        | `X26 | `W26 -> x26
        | `X27 | `W27 -> x27
        | `X28 | `W28 -> x28
        | `X29 | `W29 -> x29
        | `X30 | `W30 -> x30
        | `XZR | `WZR -> xzr
        | `SP | `WSP -> sp
        | `PC -> pc
        | `ELR -> elr
        | `SPSR -> spsr 
end

