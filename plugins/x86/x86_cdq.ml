open Core_kernel
open Bap.Std
open X86_opcode_cdq

module Make (Tools : X86_tools.S) (Backend : X86_backend.S) = struct
  open Tools

  let reg = RR.of_asm_exn

  let cdq (op:cdq) mem insn =
    let open Or_error in
    let ( ==> ) src dst =
      let src = reg src
      and dst = reg dst in
      let src_width = (RR.width src) |> Size.in_bits
      and dst_width = (RR.width dst) |> Size.in_bits in
      let extended_width = 2 * src_width in
      let extended_e = Bil.cast SIGNED extended_width @@ RR.get src in
      if dst_width = extended_width then
        RR.set dst extended_e
      else
        RR.set dst @@ Bil.cast HIGH dst_width extended_e
    in
    let stmt = match op with
      | `CDQ -> `EAX ==> `EDX
      | `CDQE -> `EAX ==> `RAX
      | `CQO -> `RAX ==> `RDX
      | `CWD -> `AX ==> `DX
      | `CWDE -> `AX ==> `EAX
      | `CBW -> `AL ==> `AX
    in
    Ok [stmt]

  let register what =
    let name op = sexp_of_cdq (op :> cdq) |> Sexp.to_string in
    List.iter (what :> cdq list)
      ~f:(fun op -> Backend.register (name op) (cdq op))

end

module IA32 = Make (X86_tools.IA32) (X86_backend.IA32)
module AMD64 = Make (X86_tools.AMD64) (X86_backend.AMD64)

let () =
  Bap_main.Extension.declare @@ fun _ctxt ->
  IA32.register all_of_cdq;
  AMD64.register all_of_cdq;
  Ok ()
