open OUnit2
open Core_kernel.Std
open Bap.Std
open Or_error
open Format
module Dis = Disasm_expert.Basic

let train filename =
  let img, errs = Image.create filename |> ok_exn in
  let arch = Image.arch img in
  let transmat = Bap_transmat_normal.totals_of_arch arch in
  Bap_transmat_normal.update_from_disasm transmat @@ disassemble_image img 

let test_training test_ctxt =
  let files = Sys.readdir (Sys.getcwd () ^ "/corpora") in
  Array.iter files ~f:(fun f -> train (Sys.getcwd () ^ "/corpora/" ^ f))

let () =
  let suite = 
  "suite">:::
    [
      "test_training">:: test_training;
    ] in
  run_test_tt_main suite
;;
