open Core_kernel.Std
open OUnit2

module Dwarf = Bap_dwarf.Std

let suite =
  "DWARF" >::: [
    Test_leb128.suite;
  ]

let run_inline_tests () =
  Ppx_inline_test_lib.Runtime.(summarize () |>
                               Test_result.record;
                               Test_result.exit ())

let () =
  if Array.mem Sys.argv "inline-test-runner"
  then run_inline_tests ()
  else run_test_tt_main suite
