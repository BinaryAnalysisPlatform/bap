open Core_kernel
open Bap_types.Std
open Bap_image_std
open Bap_core_theory

module Context = struct
  open KB.Syntax

  type t = {
    bias : Bitvec.t option;
    path : string option;
    size : int;
  }

  let is_applicable ctxt path =
    match path, ctxt.path with
    | None,_ -> true
    | Some _, None -> false
    | Some p1, Some p2 -> String.equal p1 p2

  let for_label label =
    KB.collect Arch.slot label >>= fun arch ->
    let size = Size.in_bits (Arch.addr_size arch) in
    KB.collect Theory.Label.unit label >>= function
    | None -> KB.return {bias=None; path=None; size}
    | Some unit ->
      KB.collect Theory.Unit.bias unit >>= fun bias ->
      KB.collect Theory.Unit.path unit >>| fun path -> {
        bias; path; size;
      }

  let create_addr {bias; size} ~unbiased addr =
    let bias = Option.value ~default:Bitvec.zero @@
      if unbiased then bias else None in
    Word.(create addr size - create bias size)

end

include Bap_disasm_source_intf
include Bap_disasm_source_factory
