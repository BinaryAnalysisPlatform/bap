open Core_kernel.Std
open OUnit2

module Dwarf = Bap_dwarf.Std

let suite =
  "DWARF" >::: [
    Test_leb128.suite;
  ]

(* JS is changing the inline test interface every minor release,
   so we need either wait until they stabilize it, or to move,
   to something better.  *)
let run_inline_tests () =
  eprintf "Warning: ignoring inline tests\n"

let () =
  if Array.mem Sys.argv "inline-test-runner"
  then run_inline_tests ()
  else run_test_tt_main suite
