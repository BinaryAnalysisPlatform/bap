open OUnit2

let suite =
  "DWARF" >::: [
    Test_leb128.suite;
  ]

let () =
  run_test_tt_main suite
