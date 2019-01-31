open Core_kernel
open Bap_types.Std
open Bap_image_std

module type CPU = sig
  val gpr : Var.Set.t
  val mem : var
  val sp  : var
  val zf  : var
  val cf  : var
  val vf  : var
  val nf  : var
  val is_reg : var -> bool
  val is_flag : var -> bool
  val is_sp : var -> bool
  val is_bp : var -> bool
  val is_zf : var -> bool
  val is_cf : var -> bool
  val is_vf : var -> bool
  val is_nf : var -> bool
  val is_mem : var -> bool
end

type lifter = mem -> Bap_disasm_basic.full_insn -> bil Or_error.t

module type Target = sig
  module CPU : CPU
  val lift : lifter
end
