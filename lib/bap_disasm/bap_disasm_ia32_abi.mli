open Bap_image_std
open Bap_disasm_abi

val register : abi_constructor -> unit

val create : ?merge:(abi list -> abi) -> abi_constructor
