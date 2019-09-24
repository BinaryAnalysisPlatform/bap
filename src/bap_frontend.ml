open Bap_main.Extension

module type unit = sig end

let () =
  let _unused : (module unit) = (module Bap.Std) in
  match Bap_main.init () with
  | Ok () -> ()
  | Error (Error.Exit_requested code) -> exit code
  | Error err -> Format.eprintf "%a@\n%!" Error.pp err;
    exit 1
