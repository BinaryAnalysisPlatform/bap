open Core_kernel
open OUnit2
open Bap.Std

open Powerpc_tests_helpers

let slw arch sh ctxt =
  let name = "SLW" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 24; 0] in
  let width = arch_width arch in
  let x = Word.of_int64 ~width 0xFFEEAA42_AABBCCDDL in
  let shw = Word.of_int ~width sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  if sh >= 32 then
    check_gpr init bytes r10 (Word.zero width) arch ctxt
  else
    let x = Word.extract_exn ~hi:31 Word.(x lsl shw) in
    let expected = match arch with
      | `ppc -> x
      | _ ->  Word.concat (Word.zero 32) x in
    check_gpr init bytes r10 expected arch ctxt

let srw arch sh ctxt =
  let name = "SRW" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 536; 0] in
  let width = arch_width arch in
  let x = Word.of_int64 ~width 0xFFEEAA42_AABBCCDDL in
  let shw = Word.of_int ~width sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  if sh >= 32 then
    check_gpr init bytes r10 (Word.zero width) arch ctxt
  else
    let x = Word.((extract_exn ~hi:31 x) lsr shw) in
    let expected = match arch with
      | `ppc -> x
      | _ ->  Word.concat (Word.zero 32) x in
    check_gpr init bytes r10 expected arch ctxt

let srawi arch sh x ?(carry=false) ctxt =
  let name = "SRAWI" in
  let bytes = make_insn ~name `X [31; 9; 10; sh; 824; 0] in
  let width = arch_width arch in
  let x = Word.of_int64 ~width x in
  let shw = Word.of_int ~width sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9 := int x;
    ] in
  let left_bits_num = match arch with
    | `ppc -> sh
    | _ -> 32 + sh in
  let left_bits =
    if Word.(equal (extract_exn ~hi:31 ~lo:31 x) b1) then
      Word.ones left_bits_num
    else
      Word.zero left_bits_num in
  let right_bits_num = width - left_bits_num in
  let y =  Word.extract_exn ~hi:(right_bits_num - 1) Word.(x lsr shw) in
  let expected = Word.concat left_bits y in
  let ca_expected = if carry then Word.b1 else Word.b0 in
  let ctxt = eval init bytes arch in
  let value = lookup_var ctxt r10 in
  let ca_val = lookup_var ctxt ca in
  let ca32_val = lookup_var ctxt ca32 in
  assert_bool "srawi failed: result" (is_equal_words expected value);
  assert_bool "srawi failed: ca" (is_equal_words ca_expected ca_val);
  assert_bool "srawi failed: ca32" (is_equal_words ca_expected ca32_val)

let sraw arch sh x ?(carry=false) ctxt =
  let name = "SRAW" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 792; 0] in
  let width = arch_width arch in
  let x = Word.of_int64 ~width x in
  let shw = Word.of_int ~width sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  let left_bits_num = match arch with
    | `ppc -> sh
    | _ -> 32 + sh in
  let left_bits_num = min left_bits_num width in
  let left_bits =
    if Word.(equal (extract_exn ~hi:31 ~lo:31 x) b1) then
      Word.ones left_bits_num
    else
      Word.zero left_bits_num in
  let right_bits_num = width - left_bits_num in
  let expected =
    if right_bits_num = 0 then left_bits
    else
      let right_bits =
        Word.extract_exn ~hi:(right_bits_num - 1) Word.(x lsr shw) in
      Word.concat left_bits right_bits in
  let ca_expected = if carry then Word.b1 else Word.b0 in
  let ctxt = eval init bytes arch in
  let value = lookup_var ctxt r10 in
  let ca_val = lookup_var ctxt ca in
  let ca32_val = lookup_var ctxt ca32 in
  assert_bool "sraw failed: ca" (is_equal_words ca_expected ca_val);
  assert_bool "sraw failed: ca32" (is_equal_words ca_expected ca32_val);
  assert_bool "sraw failed: result" (is_equal_words expected value)

let sld sh x ctxt =
  let arch = `ppc64 in
  let name = "SLD" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 27; 0] in
  let x = Word.of_int64 x in
  let shw = Word.of_int ~width:64 sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  let expected = Word.(x lsl shw) in
  check_gpr init bytes r10 expected arch ctxt

let srd sh x ctxt =
  let arch = `ppc64 in
  let name = "SRD" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 539; 0] in
  let x = Word.of_int64 x in
  let shw = Word.of_int ~width:64 sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  let expected = Word.(x lsr shw) in
  check_gpr init bytes r10 expected arch ctxt

let extract_from_int hi lo v =
  let w = Word.of_int ~width:32 v in
  let w'= Word.extract_exn ~hi ~lo w in
  Word.to_int_exn w'

let sradi sh x ?(carry=false) ctxt =
  let arch = `ppc64 in
  let name = "SRADI" in
  let sh0_4 = extract_from_int 4 0 sh in
  let sh5 = extract_from_int 5 5 sh in
  let bytes = make_insn ~name `XS [31; 9; 10; sh0_4; 413; sh5; 0] in
  let x = Word.of_int64 x in
  let shw = Word.of_int ~width:64 sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9 := int x;
    ] in
  let expected =
    let mask =
      if Word.(equal (extract_exn ~hi:63 ~lo:63 x) b1) then
        Word.ones 64
      else Word.zero 64 in
    let y = Word.(x lsr shw) in
    let shm = Word.of_int ~width:64 (64 - sh) in
    Word.( (mask lsl shm) lor y) in
  let ca_expected = if carry then Word.b1 else Word.b0 in
  let ctxt = eval init bytes arch in
  let value = lookup_var ctxt r10 in
  let ca_val = lookup_var ctxt ca in
  let ca32_val = lookup_var ctxt ca32 in
  assert_bool "sradi failed: result" (is_equal_words expected value);
  assert_bool "sradi failed: ca" (is_equal_words ca_expected ca_val);
  assert_bool "sradi failed: ca32" (is_equal_words ca_expected ca32_val)

let srad sh x ?(carry=false) ctxt =
  let arch = `ppc64 in
  let name = "SRAD" in
  let bytes = make_insn ~name `X [31; 9; 10; 11; 794; 0] in
  let x = Word.of_int64 x in
  let shw = Word.of_int ~width:64 sh in
  let r9  = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  let expected =
    let mask =
      if Word.(equal (extract_exn ~hi:63 ~lo:63 x) b1) then
        Word.ones 64
      else Word.zero 64 in
    let y = Word.(x lsr shw) in
    let shm = Word.of_int ~width:64 (64 - sh) in
    Word.( (mask lsl shm) lor y) in
  let ca_expected = if carry then Word.b1 else Word.b0 in
  let ctxt = eval init bytes arch in
  let value = lookup_var ctxt r10 in
  let ca_val = lookup_var ctxt ca in
  let ca32_val = lookup_var ctxt ca32 in
  assert_bool "srad failed: ca" (is_equal_words ca_expected ca_val);
  assert_bool "srad failed: ca32" (is_equal_words ca_expected ca32_val);
  assert_bool "srad failed: result" (is_equal_words expected value)

let suite = "shift" >::: [
    "slw 4"          >:: slw `ppc 4;
    "slw 0"          >:: slw `ppc 0;
    "slw zero res"   >:: slw `ppc 42;

    "srw 4"          >:: srw `ppc 4;
    "srw 0"          >:: srw `ppc 0;
    "srw zero res"   >:: srw `ppc 42;

    "srawi 4, pos,not ca" >:: srawi `ppc 4 0xFFFFFFFF_0ABBCCDDL;
    "srawi 4, pos,not ca" >:: srawi `ppc 4 0xFFFFFFFF_0ABBCCD0L;
    "srawi 4, neg,not ca" >:: srawi `ppc 4 0x00000000_8ABBCCD0L;
    "srawi 4, neg,ca"     >:: srawi `ppc 4 0x00000000_8ABBCCDDL ~carry:true;

    "sraw 4,  pos, not ca"  >:: sraw `ppc 4  0xFFFFFFFF_0ABBCCDDL;
    "sraw 32, pos, not ca"  >:: sraw `ppc 32 0xFFFFFFFF_0ABBCCDDL;
    "sraw 4,  neg, not ca"  >:: sraw `ppc 4  0xFFFFFFFF_8ABBCCD0L;
    "sraw 42, neg, ca"      >:: sraw `ppc 42 0x00000000_80000000L ~carry:true;
    "sraw 4,  neg, ca"      >:: sraw `ppc 4  0xFFFFFFFF_8ABBCCDAL ~carry:true;

    "slw64 4"          >:: slw `ppc64 4;
    "slw64 0"          >:: slw `ppc64 0;
    "slw64 zero res"   >:: slw `ppc64 42;

    "srw64 4"          >:: srw `ppc64 4;
    "srw64 0"          >:: srw `ppc64 0;
    "srw64 zero res"   >:: srw `ppc64 42;

    "srawi64 4, pos,not ca" >:: srawi `ppc64 4 0xFFFFFFFF_0ABBCCDDL;
    "srawi64 4, pos,not ca" >:: srawi `ppc64 4 0xFFFFFFFF_0ABBCCD0L;
    "srawi64 4, neg,not ca" >:: srawi `ppc64 4 0x00000000_8ABBCCD0L;
    "srawi64 4, neg,ca"     >:: srawi `ppc64 4 0x00000000_8ABBCCDDL ~carry:true;

    "sraw64 4,  pos, not ca"  >:: sraw `ppc64 4  0xFFFFFFFF_0ABBCCDDL;
    "sraw64 32, pos, not ca"  >:: sraw `ppc64 32 0xFFFFFFFF_0ABBCCDDL;
    "sraw64 4,  neg, not ca"  >:: sraw `ppc64 4  0xFFFFFFFF_8ABBCCD0L;
    "sraw64 42, neg, ca"      >:: sraw `ppc64 42 0x00000000_80000000L ~carry:true;
    "sraw64 4,  neg, ca"      >:: sraw `ppc64 4  0xFFFFFFFF_8ABBCCDAL ~carry:true;

    (* "sld64 0"   >:: sld 0  0xABCDEF42_0ABBCCDDL; *)
    (* "sld64 4"   >:: sld 4  0xABCDEF42_0ABBCCDDL; *)
    (* "sld64 42"  >:: sld 42 0xABCDEF42_0ABBCCDDL; *)

    (* "srd64 0"   >:: srd 0  0xABCDEF42_0ABBCCDDL; *)
    (* "srd64 4"   >:: srd 4  0xABCDEF42_0ABBCCDDL; *)
    (* "srd64 42"  >:: srd 42 0xABCDEF42_0ABBCCDDL; *)

    (* "sradi64 4,  neg,not ca" >:: sradi 4 0x8ABCDE42_AABBCCD0L; *)
    (* "sradi64 4,  neg, ca"    >:: sradi 4 0x8ABCDE42_AABBCCDDL ~carry:true; *)
    (* "sradi64 42, neg, ca"    >:: sradi 42 0x8ABCDE42_AABBCCDDL ~carry:true; *)
    (* "sradi64 4,  pos,not ca" >:: sradi 4 0x0ABCDE42_AABBCCDDL; *)
    (* "sradi64 42, pos,not ca" >:: sradi 42 0x0ABCDE42_AABBCCDDL; *)

    (* "srad64 4,  neg,not ca" >:: srad 4 0x8ABCDE42_AABBCCD0L; *)
    (* "srad64 4,  neg, ca"    >:: srad 4 0x8ABCDE42_AABBCCDDL ~carry:true; *)
    (* "srad64 64, neg, ca"    >:: srad 64 0x80000000_00000000L ~carry:true; *)
    (* "srad64 4,  pos,not ca" >:: srad 4 0x0ABCDE42_AABBCCDDL; *)
    (* "srad64 42, pos,not ca" >:: srad 42 0x0ABCDE42_AABBCCDDL; *)

  ]
