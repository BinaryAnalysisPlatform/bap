open Core_kernel
open OUnit2
open Bap_plugins.Std

let suite = "X86" >::: [
    Test_pshufb.suite;
    Test_pcmp.suite;
  ]


let () =
  match Bap_main.init () with
  | Error err ->
    Format.eprintf "Failed to initialize BAP: %a@\n%!"
      Bap_main.Extension.Error.pp err;
    exit 1;
  | Ok () ->
    run_test_tt_main suite
