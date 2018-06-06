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
    Test_simpl.suite ();
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


(* JS is changing the inline test interface every minor release,
   so we need either wait until they stabilize it, or to move,
   to something better.  *)
let run_inline_tests () =
  eprintf "Warning: ignoring inline tests\n"

let () = match Array.to_list Sys.argv with
  | _ :: "inline-test-runner" :: _ -> run_inline_tests ()
  | _ -> run_unit_tests ()

let pp_set pp_elem pp ppf set =
  Set.iter set ~f:(fun e -> Format.fprintf ppf "%a " pp_elem e)
