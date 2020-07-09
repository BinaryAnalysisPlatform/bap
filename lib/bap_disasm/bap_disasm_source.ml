open Core_kernel
open Bap_types.Std
open Bap_image_std
include Bap_disasm_source_intf
include Bap_disasm_source_factory


module Biased : sig
  val to_real : Bitvec.t option -> arch -> Bitvec.t -> addr
end = struct
  let to_real bias arch addr =
    let size = Size.in_bits (Arch.addr_size arch) in
    let bias = Option.value bias ~default:Bitvec.zero in
    Word.(create addr size - create bias size)
end
