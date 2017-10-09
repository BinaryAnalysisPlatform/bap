open Core_kernel.Std
open Bap.Std
open OUnit2

let suite () = "X86" >::: [
    "pshufb" >::: Test_pshufb.suite ();
  ]
