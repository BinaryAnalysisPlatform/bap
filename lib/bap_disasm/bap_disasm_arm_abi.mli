open Bap_image_std
open Bap_disasm_abi
open Bap_ir

class gnueabi : sub term -> abi

val register : abi_constructor -> unit

val create : ?merge:(abi list -> abi) -> abi_constructor
