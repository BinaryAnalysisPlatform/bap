open Core_kernel.Std
open OUnit2
open Bap_plugins.Std

let suite = "X86" >::: [
    Test_pshufb.suite;
  ]

let load_plugins () =
  Plugins.load () |>
  List.iter ~f:(function
      | Ok _ -> ()
      | Error (p,e)->
        assert_string ("failed to load plugin from " ^ p ^ ": " ^
                       Error.to_string_hum e))

let () =
  load_plugins ();
  run_test_tt_main suite
