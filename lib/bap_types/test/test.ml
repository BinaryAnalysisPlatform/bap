open OUnit

let _ = run_test_tt_main
    ("BAP" >:::
     [
       TestBitvector.tests;
       TestConceval.tests;
     ])
