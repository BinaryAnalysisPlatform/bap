open Core_kernel
open OUnit2
open Bap_plugins.Std

let suite = "PowerPC" >::: [
    Powerpc_add_tests.suite;
    Powerpc_arith_tests.suite;
    Powerpc_branch_tests.suite;
    Powerpc_compare_tests.suite;
    Powerpc_cr_tests.suite;
    Powerpc_load_tests.suite;
    Powerpc_logical_tests.suite;
    Powerpc_move_tests.suite;
    Powerpc_rotate_tests.suite;
    Powerpc_shift_tests.suite;
    Powerpc_store_tests.suite;
  ]

let load_plugins () =
  Plugins.load () |>
  List.iter ~f:(function
      | Ok _ -> ()
      | Error (p,e)->
        assert_string ("failed to load plugin from " ^ p ^ ": " ^
                       Error.to_string_hum e))

let () =
  match Bap_main.init () with
  | Error err ->
    Format.eprintf "Failed to initialize BAP: %a@\n%!"
      Bap_main.Extension.Error.pp err;
    exit 1;
  | Ok () ->
    run_test_tt_main suite
