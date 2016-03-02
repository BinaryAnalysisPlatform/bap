open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

module Basic = Bap_disasm_basic

module Kind = Bap_insn_kind

module Op  = Basic.Op
module Reg = Basic.Reg
module Fmm = Basic.Fmm
module Imm = Basic.Imm

type reg = Reg.t [@@deriving bin_io, compare, sexp]
type imm = Imm.t [@@deriving bin_io, compare, sexp]
type fmm = Fmm.t [@@deriving bin_io, compare, sexp]
type kind = Kind.t [@@deriving bin_io, compare, sexp]
