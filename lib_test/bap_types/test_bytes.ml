open Core_kernel
open Bap.Std
open Regular.Std
open OUnit2

let test_string = "This is a test to Bytes module."

let create ctxt =
  let size = 8 in
  let b = Bytes.create size in
  assert_equal ~ctxt size (Bytes.length b)

let with_string ctxt =
  let b = Bytes.of_string test_string in
  let s = Bytes.to_string b in
  assert_equal ~ctxt s test_string

let sexp ctxt =
  let b = Bytes.of_string test_string in
  let s = Bytes.sexp_of_t b in
  let b' = Bytes.t_of_sexp s in
  assert_equal ~ctxt b b'

let binp ctxt =
  let b = Bytes.of_string test_string in
  let buf = Bigstring.create (Bytes.bin_size_t b) in
  let _ = Bytes.bin_write_t buf ~pos:0 b in
  let pos_ref = ref 0 in
  let b' = Bytes.bin_read_t buf ~pos_ref in
  assert_equal ~ctxt b b'

let as_string ctxt =
  let test_bytes = Bytes.of_string test_string in
  let b = Bytes.copy test_bytes in
  assert_equal ~ctxt b test_bytes

let suite =
  "Bytes" >::: [
    "create"           >:: create;
    "of and to string" >:: with_string;
    "as_string"        >:: as_string;
    "sexp"             >:: sexp;
    "binp"             >:: binp;
  ]
