open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_abi
open Bap_disasm_abi_helpers

let registered = ref []
let register abi = registered := abi :: !registered
let create = create_abi_getter registered
