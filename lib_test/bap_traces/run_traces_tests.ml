open OUnit2

let () =
  run_test_tt_main (Test_traces.suite ())
