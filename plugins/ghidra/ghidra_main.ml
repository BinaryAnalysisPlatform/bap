open Bap_main


let () = Bap_main.Extension.declare @@ fun _ctxt ->
  Bap_ghidra.init ();
  Ok ()
