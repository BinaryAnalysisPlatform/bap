open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_abi_helpers

let lift _ _ = Or_error.error_string "not implemented"
module ABI = struct
  let register _ = ()
  let create ?merge ?image ?sym mem blk = new stub
  include Bap_disasm_abi_helpers
end

module CPU = struct
  let gpr = Var.Set.empty
  let nil = Var.create "nil" reg8_t
  let mem = nil
  let pc = nil
  let sp = nil
  let sp = nil
  let zf = nil
  let cf = nil
  let vf = nil
  let nf = nil
  let addr_of_pc = Memory.max_addr
  let no _ = false
  let is_reg = no
  let is_flag = no
  let is_sp = no
  let is_bp = no
  let is_pc = no
  let is_zf = no
  let is_cf = no
  let is_vf = no
  let is_nf = no
  let is_mem = no
end
