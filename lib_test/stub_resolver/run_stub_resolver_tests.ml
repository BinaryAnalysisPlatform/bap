open Core_kernel[@@warning "-D"]
open OUnit2

let suite = "Stub_resolver" >::: [
    Stub_resolver_tests.suite;
  ]

let () =
  match Bap_main.init () with
  | Error err ->
    Format.eprintf "Failed to initialize BAP: %a@\n%!"
      Bap_main.Extension.Error.pp err;
    exit 1;
  | Ok () ->
    run_test_tt_main suite
