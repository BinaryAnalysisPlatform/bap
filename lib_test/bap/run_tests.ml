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

let load_plugins () =
  Plugins.load () |>
  List.iter ~f:(function
      | Ok _ -> ()
      | Error (p,e)->
        assert_string ("failed to load plugin from " ^ p ^ ": " ^
                       Error.to_string_hum e))

let run_unit_tests () =
  load_plugins ();
  run_test_tt_main (suite ())

let run_inline_tests () =
  Ppx_inline_test_lib.Runtime.exit ()

let () = match Array.to_list Sys.argv with
  | _ :: "inline-test-runner" :: _ -> run_inline_tests ()
  | _ -> run_unit_tests ()

let pp_set pp_elem pp ppf set =
  Set.iter set ~f:(fun e -> Format.fprintf ppf "%a " pp_elem e)
