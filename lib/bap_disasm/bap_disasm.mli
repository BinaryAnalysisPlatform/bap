open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_types
open Image_internal_std
open Bap_disasm_brancher
open Bap_disasm_rooter

type disasm
type insn = Bap_disasm_insn.t [@@deriving bin_io, compare, sexp]
type op = Op.t [@@deriving bin_io, compare, sexp_of]
type block = Bap_disasm_block.t [@@deriving compare, sexp_of]
type cfg = Bap_disasm_rec.Cfg.t [@@deriving compare]

module Disasm : sig
  type t = disasm
  type 'a disassembler = ?backend:string -> ?brancher:brancher -> ?rooter:rooter -> 'a
  val create : cfg -> disasm
  val of_mem : (arch -> mem -> disasm Or_error.t) disassembler
  val of_image : (image -> disasm Or_error.t) disassembler
  val of_file : (?loader:string -> string -> disasm Or_error.t) disassembler

  module With_exn : sig
    val of_mem : (arch -> mem -> disasm) disassembler
    val of_image : (image -> disasm) disassembler
    val of_file : (?loader:string -> string -> disasm) disassembler
  end

  val merge : t -> t -> t
  val insns : t -> (mem * insn) seq
  val cfg : t -> cfg
  val errors : t -> Bap_disasm_rec.error list
  val insn : insn tag
end
