open Core_kernel
open OUnit2

module Leb128 = Dwarf_leb128

open Leb128

let size = 10000

let gen_uint () = Random.int (1 lsl 30 - 1)
let gen_int () = gen_uint () - gen_uint ()
let gen_uint32 () = Random.int32 Int32.max_value
let gen_int32 () = Int32.(gen_uint32 () - gen_uint32 ())
let gen_uint64 () = Random.int64 Int64.max_value
let gen_int64 () = Int64.(gen_uint64 () - gen_uint64 ())


let seq gen = Sequence.init size ~f:(fun _ -> gen ())

let ints  = seq gen_int
let uints = seq gen_uint
let ints32 = seq gen_int32
let uints32 = seq gen_uint32
let ints64 = seq gen_int64
let uints64 = seq gen_uint64

type 'a t = {
  dec : 'a Leb128.decoder;
  enc : 'a Leb128.encoder;
  signed : bool;
  show: 'a -> string;
}

let round t x ctxt =
  try match t.enc ~signed:t.signed x |> t.dec with
    | Ok y -> assert_equal ~ctxt ~printer:t.show x y
    | Error err -> assert_failure @@ Error.to_string_hum err
  with exn -> assert_failure @@ sprintf "failed on %s with %s"
      (t.show x) (Exn.to_string exn)


let rounds seq t ctxt = Sequence.iter seq  ~f:(fun x -> round t x ctxt)

let int ~signed = {
  signed;
  dec = Leb128.to_int;
  enc = Leb128.of_int;
  show = Int.to_string;
}

let int32 ~signed = {
  signed;
  dec = Leb128.to_int32;
  enc = Leb128.of_int32;
  show = Int32.to_string;
}

let int64 ~signed = {
  signed;
  dec = Leb128.to_int64;
  enc = Leb128.of_int64;
  show = Int64.to_string;
}

let leb128_printer t =
  Sexp.to_string_hum @@
  Or_error.sexp_of_t Leb128.sexp_of_t t

let read ~expect buffer t ctxt =
  assert_equal ~ctxt
    ~printer:leb128_printer
    (Leb128.read ~signed:t.signed  buffer ~pos_ref:(ref 0))
    (Ok (t.enc ~signed:t.signed expect))

let write ~expect value t ctxt =
  let leb = t.enc value in
  let buf = String.create (Leb128.size leb) in
  Leb128.write leb buf ~pos:0;
  assert_equal ~ctxt ~printer:ident buf expect

let size ~expect value t ctxt =
  let leb = t.enc value in
  assert_equal ~ctxt ~printer:Int.to_string
    (Leb128.size leb) expect


let suite = "Leb128" >::: [
    "unsinged int"   >:: rounds uints   @@ int   ~signed:false;
    "signed   int"   >:: rounds ints    @@ int   ~signed:true;
    "unsinged int32" >:: rounds uints32 @@ int32 ~signed:false;
    "signed   int32" >:: rounds ints32  @@ int32 ~signed:true;
    "unsinged int64" >:: rounds uints64 @@ int64 ~signed:false;
    "signed   int64" >:: rounds ints64  @@ int64 ~signed:true ;
    "int"            >:: rounds uints   @@ int   ~signed:true;
    "int32"          >:: rounds uints32 @@ int32 ~signed:true;
    "int64"          >:: rounds uints64 @@ int64 ~signed:false;
    "read int"       >:: read  ~expect:624485 "\xE5\x8E\x26" @@ int ~signed:false;
    "unsigned write" >:: write ~expect:"\xE5\x8E\x26" 624485 @@ int ~signed:false;
    "signed read"    >:: read  ~expect:(-624485) "\x9b\xf1\x59" @@ int ~signed:true;
    "signed write"   >:: write ~expect:"\xE5\x8E\x26" 624485 @@ int ~signed:true;
    "read int"       >:: read  ~expect:624485l "\xE5\x8E\x26" @@ int32 ~signed:false;
    "unsigned write" >:: write ~expect:"\xE5\x8E\x26" 624485l @@ int32 ~signed:false;
    "signed read"    >:: read  ~expect:(-624485l) "\x9b\xf1\x59" @@ int32 ~signed:true;
    "signed write"   >:: write ~expect:"\xE5\x8E\x26" 624485l @@ int32 ~signed:true;
    "read int"       >:: read  ~expect:624485L "\xE5\x8E\x26" @@ int64 ~signed:false;
    "unsigned write" >:: write ~expect:"\xE5\x8E\x26" 624485L @@ int64 ~signed:false;
    "signed read"    >:: read  ~expect:(-624485L) "\x9b\xf1\x59" @@ int64 ~signed:true;
    "signed write"   >:: write ~expect:"\xE5\x8E\x26" 624485L @@ int64 ~signed:true;
    "uint zero"      >:: round (int ~signed:false) 0;
    "int zero"       >:: round (int ~signed:true) 0;
    "uint32 zero"    >:: round (int32 ~signed:false) 0l;
    "int32 zero"     >:: round (int32 ~signed:true) 0l;
    "uint64 zero"    >:: round (int64 ~signed:false) 0L;
    "int64 zero"     >:: round (int64 ~signed:true) 0L;
    "uint64 zero"    >:: round (int64 ~signed:true) 0xFFFF_FFFF_FFFF_FFFDL;
    "uint zero size" >:: size ~expect:1 0 @@ int ~signed:false;
    "int zero size"  >:: size ~expect:1 0 @@ int ~signed:true;
    "uint one size"  >:: size ~expect:1 1 @@ int ~signed:false;
    "int one size"   >:: size ~expect:1 1 @@ int ~signed:true;
    "uint -1 size"   >:: size ~expect:1 1 @@ int ~signed:false;
    "int -1 size"    >:: size ~expect:1 1 @@ int ~signed:true;

  ]
