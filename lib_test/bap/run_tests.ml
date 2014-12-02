open OUnit2

let suite =
  "BAP" >::: [
    Test_bitvector.suite;
    Test_conceval.suite;
    Test_image.suite;
    Test_table.suite;
    Test_leb128.suite;
  ]


let run_unit_tests () = run_test_tt_main suite

let run_inline_tests () =
  Pa_ounit_lib.Runtime.summarize ()

let () = match Array.to_list Sys.argv with
  | _ :: "inline-test-runner" :: _ -> run_inline_tests ()
  | _ -> run_unit_tests ()
