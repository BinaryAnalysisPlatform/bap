open Core_kernel.Std
open Bap_types.Std
open Bap_image_std

module Basic = Bap_disasm_basic
module Rec = Bap_disasm_rec

module Kind = Bap_insn_kind

module Op  = Basic.Op
module Reg = Basic.Reg
module Fmm = Basic.Fmm
module Imm = Basic.Imm

type reg = Reg.t with bin_io, compare, sexp
type imm = Imm.t with bin_io, compare, sexp
type fmm = Fmm.t with bin_io, compare, sexp
type kind = Kind.t with bin_io, compare, sexp

(** A BIL model of CPU.

    In general this is a model of a processor architecture, involving
    ALU, processing unit, registers and memory.
*)
module type CPU = sig

  (** {3 Minimum set of required definitions} *)

  (** A set of general purpose registers *)
  val gpr : Var.Set.t

  (** Memory  *)
  val mem : var

  (** Program counter  *)
  val pc  : var

  (** Stack pointer  *)
  val sp  : var

  (** {4 Flag registers}  *)
  val zf  : var
  val cf  : var
  val vf  : var
  val nf  : var

  val addr_of_pc : mem -> addr

  (** {3 Predicates}  *)
  val is_reg : var -> bool
  val is_flag : var -> bool

  val is_sp : var -> bool
  val is_bp : var -> bool
  val is_pc : var -> bool

  val is_zf : var -> bool
  val is_cf : var -> bool
  val is_vf : var -> bool
  val is_nf : var -> bool
  val is_mem : var -> bool
end
