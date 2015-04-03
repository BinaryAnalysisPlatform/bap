open Core_kernel.Std
open OUnit2
open Bap_image
open Or_error


let file = "/bin/ls"

let limg,_ = ok_exn (create ~backend:"llvm_loader" file)

let bimg,_ = ok_exn (create ~backend:"bap-elf" file)

let suite = "Compare_image" >::: [
  "arch"   >::(fun test_ctxt -> assert_equal (arch limg) (arch bimg));
  "entry"  >::(fun test_ctxt ->  assert_equal (entry_point limg) (entry_point bimg));
  "segment">::(fun test_ctxt -> assert_equal (sections limg) (sections bimg));
  "symbols">::(fun test_ctxt -> todo "symbols");
  "section">::(fun test_ctxt -> todo "section");
]
