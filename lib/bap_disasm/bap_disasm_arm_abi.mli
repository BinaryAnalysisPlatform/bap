open Bap_image_std
open Bap_disasm_abi

class gnueabi : ?image:image ->
  ?sym:string -> mem -> Bap_disasm_block.t -> abi

val register : abi_constructor -> unit

val create :
  ?merge:(abi list -> abi) ->
  ?image:image ->
  ?sym:string -> mem -> Bap_disasm_block.t -> abi
