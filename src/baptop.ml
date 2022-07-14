let main () =
  UTop.require ["bap.top"];
  UTop_main.main ()


let () =
  try main () with
    Symtable.Error err ->
    Format.eprintf "Internal error: %a@." Symtable.report_error err;
    exit 1
