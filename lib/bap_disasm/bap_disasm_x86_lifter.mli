open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_types
open Bap_disasm_abi

module IA32 : sig
  module CPU : CPU
  val register_abi : abi_constructor -> unit
  val get_abi :
    ?all:bool -> (** defaults to false  *)
    ?image:image ->
    ?sym:string -> mem -> Bap_disasm_block.t -> abi list
  val lift : mem -> ('a,'k) Basic.insn -> stmt list Or_error.t
end

module AMD64 : sig
  module CPU : CPU
  val register_abi : abi_constructor -> unit
  val get_abi :
    ?all:bool -> (** defaults to false  *)
    ?image:image ->
    ?sym:string -> mem -> Bap_disasm_block.t -> abi list
  val lift : mem -> ('a,'k) Basic.insn -> stmt list Or_error.t
end
