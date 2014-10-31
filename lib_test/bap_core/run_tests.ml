open OUnit



let _ = run_test_tt_main
    ("BAP" >:::
     [
       Test_bitvector.tests;
       Test_conceval.tests;
     ])
