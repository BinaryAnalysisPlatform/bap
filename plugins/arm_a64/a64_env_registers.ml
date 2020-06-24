open Bap_core_theory
open Base
open KB.Syntax
open A64_exceptions
open A64_defs

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

  let xzr = Theory.Var.define bv64_sort "XZR"
  let sp = Theory.Var.define bv64_sort "SP"
  let pc = Theory.Var.define bv64_sort "PC"
  let elr = Theory.Var.define bv64_sort "ELR"
  let spsr = Theory.Var.define bv64_sort "SPSR"
end

