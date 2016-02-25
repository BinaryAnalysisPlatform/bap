open OUnit2

let suite = 
  "Future test" >:::
  [
    Test_future.suite;
    Test_stream.suite;
  ]
