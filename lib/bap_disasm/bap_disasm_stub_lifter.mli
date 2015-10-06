open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_types
open Bap_disasm_abi

val lift : mem -> ('a,'k) Basic.insn -> stmt list Or_error.t

module CPU : CPU

module ABI : sig
  val register : abi_constructor -> unit
  val create : ?merge:(abi list -> abi) -> abi_constructor
  include module type of Bap_disasm_abi_helpers
end
