open Core_kernel.Std
open Bap_plugins.Std

open OUnit2

let suite () =
  "BAP" >::: [
    Test_trie.suite ();
    Test_bitvector.suite ();
    Test_bili.suite ();
    Test_graph.suite ();
    Test_image.suite ();
    Test_table.suite ();
    Test_memmap.suite ();
    Test_disasm.suite ();
    Test_ir.suite ();
    Test_project.suite ();
  ]


let run_unit_tests () =
  Plugins.load () |>
  List.iter ~f:(function
      | _,Ok () -> ()
      | p,Error e ->
        assert_string ("plugin "^Plugin.name p^" failed: " ^
                       Error.to_string_hum e));
  run_test_tt_main (suite ())

let run_inline_tests () =
  Pa_ounit_lib.Runtime.summarize ()

let () = match Array.to_list Sys.argv with
  | _ :: "inline-test-runner" :: _ -> run_inline_tests ()
  | _ -> run_unit_tests ()
