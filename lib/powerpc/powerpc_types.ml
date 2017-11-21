open Bap.Std

module Dsl = Powerpc_dsl
module Model = Ppwerpc_model
module Hardware = Model.Hardware

type rtl = Dsl.rtl [@@deriving bin_io, compare, sexp]

type operand = Op.t =
  | Reg of reg
  | Imm of imm
  | Fmm of fmm
[@@deriving bin_io, compare, sexp_of]
