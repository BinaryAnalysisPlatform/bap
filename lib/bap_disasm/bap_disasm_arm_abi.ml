open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_types
open Bap_disasm_abi
open Bap_disasm_abi_helpers
open Bap_disasm_arm_env

module Block = Bap_disasm_block
module Symtab = Bap_disasm_symtab
module Insn = Bap_disasm_insn

let registered = ref []

let register abi = registered := abi :: !registered

let create =
  create_abi_getter registered
