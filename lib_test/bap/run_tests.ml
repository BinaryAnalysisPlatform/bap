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
  let open Ppx_inline_test_lib.Runtime in
  summarize () |> function
  | Test_result.Success -> ()
  | Test_result.Failure -> eprintf "Inline testing failed\n"; exit 1
  | Test_result.Error ->  eprintf "Inline testing errored\n"; exit 2

let () = match Array.to_list Sys.argv with
  | _ :: "inline-test-runner" :: _ -> run_inline_tests ()
  | _ -> run_unit_tests ()
