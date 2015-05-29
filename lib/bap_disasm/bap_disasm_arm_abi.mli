open Bap_image_std
open Bap_disasm_abi

class gnueabi : Bap_disasm_symtab.t -> Bap_disasm_symtab.fn -> abi

val register : abi_constructor -> unit

val create :
  ?merge:(abi list -> abi) ->
  Bap_disasm_symtab.t -> Bap_disasm_symtab.fn -> abi
