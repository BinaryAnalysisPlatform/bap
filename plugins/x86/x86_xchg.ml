open Core_kernel[@@warning "-D"]
open Bap.Std

let () =
  Bap_main.Extension.declare @@ fun _ctxt ->
  List.iter ["XCHG8rm"; "XCHG8rr"] ~f:(fun opcode ->
      X86_backend.IA32.register opcode X86_lifter.IA32.lift;
      X86_backend.AMD64.register opcode X86_lifter.AMD64.lift);
  Ok ()
