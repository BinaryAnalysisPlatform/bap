open OUnit2

module CamlBytes = Bytes
module type CamlBytes = module type of Bytes
module Bigstring = Core_kernel.Std.Bigstring 

open Bap.Std

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
  let b = Bytes.copy test_string in
  assert_equal ~ctxt b test_string
  
(** checks that Bap_bytes satisfies to interface of standard OCaml 
    module, except some external unsafe_* functions, that are in
    separate module, and some functions that are reimplemented 
    by included modules or replaced by labeled version. *)
module Intf_check : CamlBytes = struct
  include Bytes

  let init len f = init len ~f
  let sub s pos len = sub s ~pos ~len
  let sub_string s pos len = To_string.sub s ~pos ~len
  let iter f t = iter t ~f
  let iteri f t = iteri t ~f
  let map f t = map t ~f
  let mapi f t = mapi t ~f

  let blit src src_pos dst dst_pos len = 
    blit ~src ~src_pos ~dst ~dst_pos ~len

  let blit_string src src_pos dst dst_pos len =
    To_string.blit ~src ~src_pos ~dst ~dst_pos ~len

  let unsafe_of_string = Unsafe.of_string
  let unsafe_to_string = Unsafe.to_string

  external unsafe_get: t -> int -> char = "%string_unsafe_get"
  external unsafe_set: t -> int -> char -> unit = "%string_unsafe_set"
  external unsafe_fill: t -> int -> int -> char -> unit =
    "caml_fill_string" "noalloc"
  external unsafe_blit: t -> int -> t -> int -> int -> unit =
    "caml_blit_string" "noalloc"
end

let suite =
  "Bytes" >::: [
    "create"           >:: create;
    "of and to string" >:: with_string;
    "as_string"        >:: as_string;
    "sexp"             >:: sexp;
    "binp"             >:: binp;
  ]
