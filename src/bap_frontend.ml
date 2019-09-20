let main () = Bap_main.init ()
module Link_bap_main_package = Bap.Std

let () = match main () with
  | Ok () -> ()
  | Error err ->
    Format.eprintf "%a@\n%!"
      Bap_main.Extension.Error.pp err;
    exit 1
