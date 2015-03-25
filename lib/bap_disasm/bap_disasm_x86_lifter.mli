open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_types
open Bap_disasm_abi

module IA32 : sig
  module CPU : sig
    include module type of Bap_disasm_x86_env
    include Bap_disasm_x86_env.ModeVars
    include CPU
  end
  val lift : mem -> ('a,'k) Basic.insn -> stmt list Or_error.t
end

module AMD64 : sig
  module CPU : sig
    include module type of Bap_disasm_x86_env
    include Bap_disasm_x86_env.ModeVars
    include CPU
  end
  val lift : mem -> ('a,'k) Basic.insn -> stmt list Or_error.t
end
