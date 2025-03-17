open Core_kernel[@@warning "-D"]
open OUnit2

module Dwarf = Bap_dwarf.Std
module Sys = Stdlib.Sys

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
  if Array.mem ~equal:String.equal Sys.argv "inline-test-runner"
  then run_inline_tests ()
  else run_test_tt_main suite
