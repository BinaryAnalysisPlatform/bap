open OUnit2
open Core_kernel
open Regular.Std
open Bap.Std

let deadbeef = Word.(of_int32 0xDEADBEEFl)
let _DEADBEEF = Word.(of_int64 0xDEADBEEFL)
let zero_32 = Word.(of_int32 0x0l)
let zero_64 = Word.(of_int64 0x0L)

let normalized ~n ~add ~width ctxt =
  assert_equal ~ctxt
    ~printer:Word.to_string
    (Word.of_int n ~width)
    (Word.of_int (n + add) ~width)

let to_string ~str v ctxt =
  assert_equal ~ctxt ~printer:ident str (Word.to_string v)

let of_string ~str v ctxt =
  assert_equal ~ctxt ~printer:Word.to_string
    v (Word.of_string str)

let print_int_list lst =
  let sexp_of_int n = Sexp.Atom (sprintf "0x%02X" n) in
  Sexp.to_string_hum (sexp_of_list sexp_of_int lst)

let to_chars ~expect endian v ctxt =
  assert_equal ~ctxt
    ~printer:print_int_list
    expect Word.(enum_chars endian v    |>
                 Seq.map ~f:Char.to_int |>
                 Sequence.to_list)

let concat ~expect v1 v2 ctxt =
  assert_equal ~ctxt ~printer:Word.to_string
    expect Word.(v1 @. v2)

let is_zero word ctxt =
  assert_bool Word.(to_string word) Word.(is_zero b0)

let validate validate word ctxt =
  let v = validate word in
  assert_bool
    (String.concat ~sep:"\n" (Validate.errors v))
    (Validate.result v = Ok ())

let binary op ~width ~expect x y ctxt =
  let (!$) = Word.of_int ~width in
  assert_equal ~ctxt
    ~printer:Word.to_string
    ~cmp:Word.equal
    !$expect (op !$x !$y)

let binary3 op ~width ~expect x y ctxt =
  let (!$) = Word.of_int ~width in
  let (!$$$) (a, b, c) = (!$a, !$b, !$c) in
  let equal (a, b, c) (a', b', c') =
    Word.equal a a' &&
    Word.equal b b' &&
    Word.equal c c' in
  let to_string (a, b, c) =
    Printf.sprintf "(%s,%s,%s)"
      (Word.to_string a)
      (Word.to_string b)
      (Word.to_string c) in
  assert_equal ~ctxt
    ~printer:to_string
    ~cmp:equal
    !$$$expect (op !$x !$y)

let sub = binary Word.Int_exn.sub
let lshift = binary Word.Int_exn.lshift
let rshift = binary Word.Int_exn.rshift
let arshift = binary Word.Int_exn.arshift
let gcd = binary Word.gcd_exn
let lcm = binary Word.lcm_exn
let gcdext = binary3 Word.gcdext_exn

let is yes ctxt = assert_bool "doesn't hold" yes

let lognot x y ~width =
  is Word.(Int_exn.lnot (of_int x ~width) = of_int y ~width)

let bitsub ?hi ?lo ~expect v ctxt =
  let (!$) (z,w) = Word.of_int z ~width:w in
  assert_equal ~ctxt
    ~printer:Word.to_string
    !$expect
    (Word.extract_exn ?hi ?lo !$v)

let of_binary endian string ~expect ctxt =
  assert_equal ~ctxt
    ~printer:Word.to_string
    expect
    (Word.of_binary endian string)


let suite () =
  "Bitvector" >:::
  [
    "13=13+1024|7"   >:: normalized ~n:13 ~add:1024 ~width:7;
    "13=13-1024|7"   >:: normalized ~n:13 ~add:(-1024) ~width:7;
    "3=3+4|2"        >:: normalized ~n:3 ~add:4 ~width:2;
    "3=3-4|2"        >:: normalized ~n:3 ~add:(-4) ~width:2;
    "to_string:b0"   >:: to_string ~str:"0:1u"  Word.b0;
    "to_string:b1"   >:: to_string ~str:"1:1u"  Word.b1;
    "to_string:7|4"  >:: to_string ~str:"7:4u" Word.(of_int 7 ~width:4);
    "to_string:7|3"  >:: to_string ~str:"7:3u" Word.(of_int 7 ~width:3);
    "to_string:7|2"  >:: to_string ~str:"3:2u" Word.(of_int 7 ~width:2);
    "to_string:-1|3" >:: to_string ~str:"7:3u" Word.(of_int (-1) ~width:3);
    "to_string:-1|3" >:: to_string ~str:"-1:3s" Word.(signed @@ of_int (-1) ~width:3);
    "of_string:b0"   >:: of_string ~str:"false" Word.b0;
    "of_string:b1"   >:: of_string ~str:"true"  Word.b1;
    "of_string:b0"   >:: of_string ~str:"0:1u" Word.b0;
    "of_string:b1"   >:: of_string ~str:"1:1u"  Word.b1;
    "of_string:b0"   >:: of_string ~str:"0:1" Word.b0;
    "of_string:b1"   >:: of_string ~str:"1:1"  Word.b1;
    "of_string:7|4"  >:: of_string ~str:"7:4u" Word.(of_int 7 ~width:4);
    "of_string:7|3"  >:: of_string ~str:"7:3u" Word.(of_int 7 ~width:3);
    "of_string:7|2"  >:: of_string ~str:"3:2u" Word.(of_int 7 ~width:2);
    "of_string:-1|3" >:: of_string ~str:"7:3u" Word.(of_int (-1) ~width:3);
    "of_string:7|4"  >:: of_string ~str:"7:4" Word.(of_int 7 ~width:4);
    "of_string:7|3"  >:: of_string ~str:"7:3" Word.(of_int 7 ~width:3);
    "of_string:7|2"  >:: of_string ~str:"3:2" Word.(of_int 7 ~width:2);
    "of_string:-1|3" >:: of_string ~str:"7:3" Word.(of_int (-1) ~width:3);

    "chars:0|64le" >:: to_chars Word.(of_int 0 ~width:64)  LittleEndian
      ~expect:[0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0];
    "chars:0|64be" >:: to_chars Word.(of_int 0 ~width:64)  BigEndian
      ~expect:[0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0];
    "chars:0|32le" >:: to_chars Word.(of_int 0 ~width:32)  LittleEndian
      ~expect:[0x0; 0x0; 0x0; 0x0];
    "chars:0|32be" >:: to_chars Word.(of_int 0 ~width:32)  BigEndian
      ~expect:[0x0; 0x0; 0x0; 0x0];
    "chars:1|128le" >:: to_chars Word.(of_int 1 ~width:128)  LittleEndian
      ~expect:[0x1; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0;
               0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0];
    "chars:1|128be" >:: to_chars Word.(of_int 1 ~width:128)  BigEndian
      ~expect:[0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0;
               0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x1];
    "chars:1|64le" >:: to_chars Word.(of_int 1 ~width:64)  LittleEndian
      ~expect:[0x1; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0];
    "chars:1|64be" >:: to_chars Word.(of_int 1 ~width:64)  BigEndian
      ~expect:[0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x1];
    "chars:1|32le" >:: to_chars Word.(of_int 1 ~width:32)  LittleEndian
      ~expect:[0x1; 0x0; 0x0; 0x0];
    "chars:1|32be" >:: to_chars Word.(of_int 1 ~width:32)  BigEndian
      ~expect:[0x0; 0x0; 0x0; 0x1];
    "chars:1|16le" >:: to_chars Word.(of_int 1 ~width:16)  LittleEndian
      ~expect:[0x1; 0x0];
    "chars:1|16be" >:: to_chars Word.(of_int 1 ~width:16)  BigEndian
      ~expect:[0x0; 0x1];
    "chars:1|8le" >:: to_chars Word.(of_int 1 ~width:8)  LittleEndian
      ~expect:[0x1];
    "chars:1|8be" >:: to_chars Word.(of_int 1 ~width:8)  BigEndian
      ~expect:[0x1];
    "chars:1|4le" >:: to_chars Word.(of_int 1 ~width:4)  LittleEndian
      ~expect:[0x1];
    "chars:1|4be" >:: to_chars Word.(of_int 1 ~width:4)  BigEndian
      ~expect:[0x1];
    "chars:1|1le" >:: to_chars Word.(of_int 1 ~width:1)  LittleEndian
      ~expect:[0x1];
    "chars:1|1be" >:: to_chars Word.(of_int 1 ~width:1)  BigEndian
      ~expect:[0x1];
    "chars:1|7le" >:: to_chars Word.(of_int 1 ~width:7)  LittleEndian
      ~expect:[0x1];
    "chars:1|7be" >:: to_chars Word.(of_int 1 ~width:7)  BigEndian
      ~expect:[0x1];
    "chars:1|13le" >:: to_chars Word.(of_int 1 ~width:13)  LittleEndian
      ~expect:[0x1; 0x0];
    "chars:1|13be" >:: to_chars Word.(of_int 1 ~width:13)  BigEndian
      ~expect:[0x0; 0x1];
    "chars:1|71le" >:: to_chars Word.(of_int 1 ~width:71)  LittleEndian
      ~expect:[0x1; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0];
    "chars:1|71be" >:: to_chars Word.(of_int 1 ~width:71)  BigEndian
      ~expect:[0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x1];
    "chars:DEADBEEF|32" >:: to_chars deadbeef LittleEndian
      ~expect:[0xEF; 0xBE; 0xAD; 0xDE];
    "chars:DEADBEEF|32" >:: to_chars deadbeef BigEndian
      ~expect:[0xDE; 0xAD; 0xBE; 0xEF];

    "of_binary:be" >:: of_binary BigEndian "\xDE\xAD\xBE\xEF" ~expect:deadbeef;
    "of_binary:le" >:: of_binary LittleEndian "\xEF\xBE\xAD\xDE" ~expect:deadbeef;

    "concat:DEADBEEF|16+16" >:: concat ~expect:deadbeef
      Word.(of_int 0xDEAD ~width:16)
      Word.(of_int 0xBEEF ~width:16);

    "concat:DEADBEEF|8+24" >:: concat ~expect:deadbeef
      Word.(of_int 0xDE ~width:8)
      Word.(of_int 0xADBEEF ~width:24);

    "concat:DEADBEEF|4+28" >:: concat ~expect:deadbeef
      Word.(of_int 0x0D ~width:4)
      Word.(of_int 0x0EADBEEF ~width:28);

    "concat:D7|3+5" >:: concat ~expect:Word.(of_int 0xD7 ~width:8)
      Word.(of_int 0x6  ~width:3)
      Word.(of_int 0x17 ~width:5);

    "with_zero:0" >:: is_zero Word.b0;
    "with_zero:1*0" >:: is_zero Word.(Int_exn.(b0 * b1));
    "with_zero:1-1" >:: is_zero Word.(Int_exn.(b1 - b1));
    "with_zero:255+1" >:: is_zero
      Word.(Int_exn.(of_int 255 ~width:8 + of_int 1 ~width:8));

    "validate_positive:1"     >:: validate Word.validate_positive Word.b1;
    "validate_non_negative:1" >:: validate Word.validate_non_negative Word.b1;
    "validate_non_negative:0" >:: validate Word.validate_non_negative Word.b0;
    "validate_non_positive:0" >:: validate Word.validate_non_positive Word.b0;

    "validate_positive:-1u" >:: (* by default all numbers are unsigned *)
    validate Word.validate_positive Word.(Int_exn.(~-b1));

    "validate_negative:-1s" >::
    validate Word.validate_negative Word.(signed Int_exn.(~-b1));

    "validate_negative:1s" >::  (* yes, in 2-complement there is no +1 *)
    validate Word.validate_negative Word.(signed Int_exn.(b1));

    "validate_positive:deadbeef" >:: validate Word.validate_positive deadbeef;

    "validate_positive:-deadbeef" >::
    validate Word.validate_positive Word.Int_exn.(~-deadbeef);

    "validate_positive:-deadbeefs" >:: (*  not enough bits! *)
    validate Word.validate_positive Word.(signed Int_exn.(~-deadbeef));

    "validate_negative:-DEADBEEFs" >::
    validate Word.validate_negative Word.(signed Int_exn.(~-_DEADBEEF));

    "is_positive:1"     >:: is Word.(b1 > b0);
    "is_non_negative:1" >:: is Word.(b1 >= b0);
    "is_non_negative:0" >:: is Word.(b0 >= b0);
    "is_non_positive:0" >:: is Word.(b0 <= b0);
    "is_positive:-1u" >:: is Word.(Int_exn.(~-b1) > b0);
    "validate_negative:-1s" >:: is Word.(signed Int_exn.(~-b1) < b0);
    "validate_negative:1s" >:: is Word.(signed Int_exn.(b1) < b0);
    "is_positive:deadbeef" >:: is Word.(deadbeef > zero_32);
    "is_positive:-deadbeef" >:: is Word.(Int_exn.(~-deadbeef) > zero_32);
    "is_positive:-deadbeefs" >:: is Word.(signed Int_exn.(~-deadbeef) > zero_32);
    "is_positive:DEADBEEF"   >:: is Word.(_DEADBEEF > zero_64);
    "is_positive:-DEADBEEF"  >:: is Word.(Int_exn.(~-_DEADBEEF) > zero_64);
    "is_negative:-DEADBEEFs" >:: is Word.(signed Int_exn.(~-_DEADBEEF) < zero_64);
    "lognot:13" >:: lognot 13 ~-14 ~width:8;
    "lognot:1398765" >:: lognot 1398765 ~-1398766 ~width:4;
    "lognot:13" >:: lognot 0 ~-1 ~width:4;
    "sub" >:: sub ~width:8 ~expect:0xFF 0 1;
    "lshift" >:: lshift ~width:8 ~expect:0x0 0x1 0xA ;
    "rshift" >:: rshift ~width:8 ~expect:0x3f 0xFF 0x2 ;
    "arshift" >:: arshift ~width:8 ~expect:0xff 0xFF 0x2 ;
    "gcd" >:: gcd ~width:8 ~expect:0x4 0x10 0xC ;
    "gcd" >:: gcd ~width:8 ~expect:0x1 0x11 0xF ;
    "lcm" >:: lcm ~width:8 ~expect:0x30 0x10 0xC ;
    "lcm" >:: lcm ~width:8 ~expect:0x8C 0x1C 0x23 ;
    "gcdext" >:: gcdext ~width:8 ~expect:(0x4,0x1,-0x1) 0x10 0xC ;

    (* a small cheatsheet for a bit numbering *)
    (** D    A    D    5    *)
    (** FEDC_BA98_7654_3210 *)
    "cast_high:4" >:: bitsub ~expect:(0xD,4)  ~lo:0xC (0xDAD5,16);
    "cast_high:8" >:: bitsub ~expect:(0xDA,8) ~lo:0x8 (0xDAD5,16);
    "cast_low:8"  >:: bitsub ~expect:(0xD5,8) ~hi:0x7 (0xDAD5,16);
    "cast_low:4"  >:: bitsub ~expect:(0x5,4)  ~hi:0x3 (0xDAD5,16);
    "cast_mid:8"  >:: bitsub ~expect:(0xAD,8) ~hi:0xB ~lo:0x4 (0xDAD5,16);
    "mono_size"   >:: (fun ctxt ->
        try
          ignore Word.(Mono.(zero_32 < b0));
          assert_string "Monomorphic comparison"
        with exn -> ());
  ]
