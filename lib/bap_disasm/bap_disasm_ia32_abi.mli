open Bap_image_std
open Bap_disasm_abi

val register : abi_constructor -> unit

val create :
  ?merge:(abi list -> abi) ->
  ?image:image ->
  ?sym:string -> mem -> Bap_disasm_block.t -> abi
