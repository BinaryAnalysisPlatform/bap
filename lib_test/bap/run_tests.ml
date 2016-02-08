open OUnit2

let suite =
  "BAP" >::: [
    Test_trie.suite;
    Test_bitvector.suite;
    Test_bili.suite;
    Test_bytes.suite;
    Test_graph.suite;
    Test_image.suite;
    Test_table.suite;
    Test_memmap.suite;
    Test_leb128.suite;
    Test_disasm.suite;
    Test_ir.suite;
    Test_project.suite;
    Test_llvm_loader.suite;
    Test_trace.suite;
  ]


let run_unit_tests () = run_test_tt_main suite

let run_inline_tests () =
  Pa_ounit_lib.Runtime.summarize ()

let () = match Array.to_list Sys.argv with
  | _ :: "inline-test-runner" :: _ -> run_inline_tests ()
  | _ -> run_unit_tests ()
