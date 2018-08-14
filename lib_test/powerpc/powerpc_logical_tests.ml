open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_tests_helpers

let andi_dot arch ctxt =
  let bytes = "\x71\x2a\x00\x1F" in  (** andi.   r10,r9,31 *)
  let r10 = find_gpr arch "R10" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let init = Bil.[
      r9 := int (Word.of_int ~width 10);
    ] in
  let expected = Word.of_int ~width 10 in
  check_gpr init bytes r10 expected arch ctxt

let andis_dot arch ctxt =
  let bytes = "\x75\x2a\x0E\x00" in  (** andis.  r10,r9,2048 *)
  let r10 = find_gpr arch "R10" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let value = Word.of_int ~width 0x0800_0000 in
  let init = Bil.[
      r9 := int value;
    ] in
  let expected = value in
  check_gpr init bytes r10 expected arch ctxt

let and_ arch ctxt =
  let bytes = "\x7f\x39\xe8\x38" in (** and r25 r25 r29 *)
  let r25 = find_gpr arch "R25" in
  let r29 = find_gpr arch "R29" in
  let width = arch_width arch in
  let x = 31 in
  let y = 10 in
  let r = x land y in
  let init = Bil.[
      r29  := int (Word.of_int ~width x);
      r25 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r25 expected arch ctxt

let and_dot arch ctxt =
  let bytes = "\x7f\x39\xe8\x39" in (** and. r25 r25 r29 *)
  let r25 = find_gpr arch "R25" in
  let r29 = find_gpr arch "R29" in
  let width = arch_width arch in
  let x = 31 in
  let y = 10 in
  let r = x land y in
  let init = Bil.[
      r29  := int (Word.of_int ~width x);
      r25 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r25_value = lookup_var ctxt r25 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "add. failed" (is_equal_words expected r25_value);
  assert_bool "addc. failed" (is_equal_words Word.b0 nf_value);
  assert_bool "addc. failed" (is_equal_words Word.b1 pf_value);
  assert_bool "addc. failed" (is_equal_words Word.b0 zf_value)

let andc arch ctxt =
  let bytes = "\x7c\xea\x50\x78" in (** andc r10 r7 r10 *)
  let r10 = find_gpr arch "R10" in
  let r7  = find_gpr arch "R7" in
  let width = arch_width arch in
  let x = 21 in
  let y = 10 in
  let r = x land (lnot y) in
  let init = Bil.[
      r7 := int (Word.of_int ~width x);
      r10 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r10 expected arch ctxt

let andc_dot arch ctxt =
  let bytes = "\x7c\xea\x50\x79" in (** andc. r10 r7 r10 *)
  let r10 = find_gpr arch "R10" in
  let r7  = find_gpr arch "R7" in
  let width = arch_width arch in
  let x = 42 in
  let y = x in
  let r = x land (lnot y) in
  let init = Bil.[
      r7 := int (Word.of_int ~width x);
      r10 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r10_value = lookup_var ctxt r10 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "addc. failed" (is_equal_words expected r10_value);
  assert_bool "addc. failed" (is_equal_words Word.b0 nf_value);
  assert_bool "addc. failed" (is_equal_words Word.b0 pf_value);
  assert_bool "addc. failed" (is_equal_words Word.b1 zf_value)

let ori arch ctxt =
  let bytes = "\x60\xc6\x51\xc1" in  (** ori     r6,r6,20929 *)
  let r6 = find_gpr arch "R6" in
  let width = arch_width arch in
  let x = 62 in
  let r = x lor 20929 in
  let init = Bil.[
      r6 := int (Word.of_int ~width x);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r6 expected arch ctxt

let oris arch ctxt =
  let bytes = "\x65\x4a\x00\x0F" in (** oris    r10,r10,15  *)
  let r10 = find_gpr arch "R10" in
  let width = arch_width arch in
  let x = 61440 in
  let y = 15 in
  let r = x lxor (y lsl 16) in
  let init = Bil.[
      r10 := int (Word.of_int ~width x);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r10 expected arch ctxt

let or_  arch ctxt =
  let bytes = "\x7f\x38\xc3\x78" in (** or r24,r25,r24  *)
  let r24 = find_gpr arch "R24" in
  let r25 = find_gpr arch "R25" in
  let width = arch_width arch in
  let x = 24 in
  let y = 10 in
  let r = x lor y in
  let init = Bil.[
      r24 := int (Word.of_int ~width x);
      r25 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r24 expected arch ctxt

let or_dot arch ctxt =
  let bytes = "\x7f\x38\xc3\x79" in (** or r24,r25,r24  *)
  let r24 = find_gpr arch "R24" in
  let r25 = find_gpr arch "R25" in
  let width = arch_width arch in
  let x = 24 in
  let y = 10 in
  let r = x lor y in
  let init = Bil.[
      r24 := int (Word.of_int ~width x);
      r25 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r24_value = lookup_var ctxt r24 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "or. failed" (is_equal_words expected r24_value);
  assert_bool "or. failed" (is_equal_words Word.b0 nf_value);
  assert_bool "or. failed" (is_equal_words Word.b1 pf_value);
  assert_bool "or. failed" (is_equal_words Word.b0 zf_value)

let orc arch ctxt =
  let bytes = "\x7c\x8a\x53\x38" in (** orc     r10,r4,r10 *)
  let r4 = find_gpr arch "R4" in
  let r10 = find_gpr arch "R10" in
  let width = arch_width arch in
  let x = 42 in
  let y = 10 in
  let r = x lor (lnot y)  in
  let init = Bil.[
      r4 := int (Word.of_int ~width x);
      r10 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r10 expected arch ctxt

let orc_dot arch ctxt =
  let bytes = "\x7c\x8a\x53\x39" in (** orc.     r10,r4,r10 *)
  let r4 = find_gpr arch "R4" in
  let r10 = find_gpr arch "R10" in
  let width = arch_width arch in
  let x = 42 in
  let y = 0 in
  let r = x lor (lnot y)  in
  let init = Bil.[
      r4 := int (Word.of_int ~width x);
      r10 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r10_value = lookup_var ctxt r10 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "orc. failed" (is_equal_words expected r10_value);
  assert_bool "orc. failed" (is_equal_words Word.b1 nf_value);
  assert_bool "orc. failed" (is_equal_words Word.b0 pf_value);
  assert_bool "orc. failed" (is_equal_words Word.b0 zf_value)

let xori arch ctxt =
  let bytes = "\x68\x63\x00\x0B" in (** xori    r3,r3,11   *)
  let r3 = find_gpr arch "R3" in
  let width = arch_width arch in
  let x = 16 in
  let r = x lxor 11 in
  let init = Bil.[
      r3 := int (Word.of_int ~width x);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r3 expected arch ctxt

let xoris arch ctxt =
  let bytes = "\x6d\x2a\x00\x0f" in (** xoris r10,r9,15 *)
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let width = arch_width arch in
  let x = 0x1F0000 in
  let r = x lxor (15 lsl 16) in
  let init = Bil.[
      r9 := int (Word.of_int ~width x);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r10 expected arch ctxt

let xor_ arch ctxt =
  let bytes = "\x7c\x6a\x52\x78" in (** xor     r10,r3,r10 *)
  let r3 = find_gpr arch "R3" in
  let r10 = find_gpr arch "R10" in
  let width = arch_width arch in
  let x = 42 in
  let y = 15 in
  let r = x lxor y in
  let init = Bil.[
      r3 := int (Word.of_int ~width x);
      r10 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r10 expected arch ctxt

let xor_dot arch ctxt =
  let bytes = "\x7c\x6a\x52\x79" in (** xor.     r10,r3,r10 *)
  let r3 = find_gpr arch "R3" in
  let r10 = find_gpr arch "R10" in
  let width = arch_width arch in
  let x = 42 in
  let y = 42 in
  let r = x lxor y in
  let init = Bil.[
      r3 := int (Word.of_int ~width x);
      r10 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r10_value = lookup_var ctxt r10 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "xor. failed" (is_equal_words expected r10_value);
  assert_bool "xor. failed" (is_equal_words Word.b0 nf_value);
  assert_bool "xor. failed" (is_equal_words Word.b0 pf_value);
  assert_bool "xor. failed" (is_equal_words Word.b1 zf_value)

let nand arch ctxt =
  let bytes = "\x7c\x63\x23\xb8" in (** nand    r3,r3,r4 *)
  let r3 = find_gpr arch "R3" in
  let r4 = find_gpr arch "R4" in
  let width = arch_width arch in
  let x = 42 in
  let y = 15 in
  let r = lnot (x land y) in
  let init = Bil.[
      r3 := int (Word.of_int ~width x);
      r4 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r3 expected arch ctxt

let nand_dot arch ctxt =
  let bytes = "\x7c\x63\x23\xb9" in (** nand.    r3,r3,r4 *)
  let r3 = find_gpr arch "R3" in
  let r4 = find_gpr arch "R4" in
  let width = arch_width arch in
  let x = 42 in
  let y = 15 in
  let r = lnot (x land y) in
  let init = Bil.[
      r3 := int (Word.of_int ~width x);
      r4 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r3_value = lookup_var ctxt r3 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "nand. failed" (is_equal_words expected r3_value);
  assert_bool "nand. failed" (is_equal_words Word.b1 nf_value);
  assert_bool "nand. failed" (is_equal_words Word.b0 pf_value);
  assert_bool "nand. failed" (is_equal_words Word.b0 zf_value)

let nor arch ctxt =
  let bytes = "\x7d\x09\x48\xf8" in (** nor     r9,r8,r9 *)
  let r8 = find_gpr arch "R8" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let x = 42 in
  let y = 15 in
  let r = lnot (x lor y) in
  let init = Bil.[
      r8 := int (Word.of_int ~width x);
      r9 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r9 expected arch ctxt

let nor_dot arch ctxt =
  let bytes = "\x7d\x09\x48\xf9" in (** nor.     r9,r8,r9 *)
  let r8 = find_gpr arch "R8" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let x = 42 in
  let y = -15 in
  let r = lnot (x lor y) in
  let init = Bil.[
      r8 := int (Word.of_int ~width x);
      r9 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r9_value = lookup_var ctxt r9 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "nor. failed" (is_equal_words expected r9_value);
  assert_bool "nor. failed" (is_equal_words Word.b0 nf_value);
  assert_bool "nor. failed" (is_equal_words Word.b1 pf_value);
  assert_bool "nor. failed" (is_equal_words Word.b0 zf_value)

let eqv arch ctxt =
  let bytes = "\x7d\x09\x4a\x38" in (** eqv     r9,r8,r9 *)
  let r8 = find_gpr arch "R8" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let x = 42 in
  let y = 15 in
  let r = lnot (x lxor y); in
  let init = Bil.[
      r8 := int (Word.of_int ~width x);
      r9 := int (Word.of_int ~width y);
    ] in
  let expected = Word.of_int ~width r in
  check_gpr init bytes r9 expected arch ctxt

let eqv_dot arch ctxt =
  let bytes = "\x7d\x09\x4a\x39" in (** eqv.     r9,r8,r9 *)
  let r8 = find_gpr arch "R8" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let x = -1 in
  let y = 0 in
  let r = lnot (x lxor y); in
  let init = Bil.[
      r8 := int (Word.of_int ~width x);
      r9 := int (Word.of_int ~width y);
    ] in
  let ctxt = eval init bytes arch in
  let r9_value = lookup_var ctxt r9 in
  let expected = Word.of_int ~width r in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "eqv. failed" (is_equal_words expected r9_value);
  assert_bool "eqv. failed" (is_equal_words Word.b0 nf_value);
  assert_bool "eqv. failed" (is_equal_words Word.b0 pf_value);
  assert_bool "eqv. failed" (is_equal_words Word.b1 zf_value)

let extsb arch ctxt =
  let bytes = "\x7d\x6a\x07\x74" in   (** extsb   r10,r11  *)
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let width = arch_width arch in
  let init = Bil.[
      r11 := int (Word.of_int ~width 0xC0);
    ] in
  let expected = Word.of_int64 ~width 0xFFFFFFFFFFFFFFC0L in
  check_gpr init bytes r10 expected arch ctxt

let extsh arch ctxt =
  let bytes = "\x7d\x25\x07\x34" in   (** extsh   r5,r9 *)
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let init = Bil.[
      r9 := int (Word.of_int ~width 0xC000);
    ] in
  let expected = Word.of_int64 ~width 0xFFFFFFFFFFFFC000L in
  check_gpr init bytes r5 expected arch ctxt

let exts_dot arch ctxt =
  let bytes = "\x7d\x25\x07\x35" in   (** extsh.   r5,r9 *)
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let init = Bil.[
      r9 := int (Word.of_int ~width 0xC000);
    ] in
  let expected = Word.of_int64 ~width 0xFFFFFFFFFFFFC000L in
  let ctxt = eval init bytes arch in
  let r5_value = lookup_var ctxt r5 in
  let nf_value = lookup_var ctxt nf in
  let pf_value = lookup_var ctxt pf in
  let zf_value = lookup_var ctxt zf in
  assert_bool "extsh. failed" (is_equal_words expected r5_value);
  assert_bool "extsh. failed" (is_equal_words Word.b1 nf_value);
  assert_bool "extsh. failed" (is_equal_words Word.b0 pf_value);
  assert_bool "extsh. failed" (is_equal_words Word.b0 zf_value)

let extsw arch ctxt =
  let bytes = "\x7d\x25\x07\xb4" in   (** extsw   r5,r9 *)
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let init = Bil.[
      r9 := int (Word.of_int ~width 0xC0000000);
    ] in
  let expected = Word.of_int64 ~width 0xFFFFFFFFC0000000L in
  check_gpr init bytes r5 expected arch ctxt

let extsw_dot arch ctxt =
  let bytes = "\x7d\x25\x07\xb5" in   (** extsw.  r5,r9 *)
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let init = Bil.[
      r9 := int (Word.of_int ~width 0xC0000000);
    ] in
  let expected = Word.of_int64 ~width 0xFFFFFFFFC0000000L in
  check_gpr init bytes r5 expected arch ctxt

let cntlzw arch value zeros ctxt =
  let bytes = "\x7c\x63\x00\x34" in
  let r = find_gpr arch "R3" in
  let width = arch_width arch in
  let init = Bil.[
      r := int (Word.of_int ~width value);
    ] in
  let expected = Word.of_int ~width zeros in
  check_gpr init bytes r expected arch ctxt

let cnttzw arch value zeros ctxt =
  let bytes = "\x7c\x63\x04\x34" in
  let r = find_gpr arch "R3" in
  let width = arch_width arch in
  let init = Bil.[
      r := int (Word.of_int ~width value);
    ] in
  let expected = Word.of_int ~width zeros in
  check_gpr init bytes r expected arch ctxt

let cntlzd ctxt =
  let arch = `ppc64 in
  let bytes = "\x7c\x63\x00\x74" in
  let r = find_gpr arch "R3" in
  let width = arch_width arch in
  let value = Word.of_int64 0x0000FFFF_00000000L in
  let init = Bil.[
      r := int value;
    ] in
  let expected = Word.of_int ~width 16 in
  check_gpr init bytes r expected arch ctxt

let cnttzd ctxt =
  let arch = `ppc64 in
  let bytes = "\x7c\x63\x04\x75" in
  let r = find_gpr arch "R3" in
  let width = arch_width arch in
  let value = Word.of_int64 0x00000000_FFFF0000L in
  let init = Bil.[
      r := int value;
    ] in
  let expected = Word.of_int ~width 16 in
  check_gpr init bytes r expected arch ctxt

let cmpb arch ~bytes_cnt x y expected ctxt =
  let bytes = "\x7c\x8a\x53\xf8" in
  let width = arch_width arch in
  let x = Word.of_int ~width x in
  let y = Word.of_int ~width y in
  let r10 = find_gpr arch "R10" in
  let r4 = find_gpr arch "R4" in
  let init = Bil.[
      r10 := int x;
      r4  := int y;
    ] in
  let head = Word.ones (width - bytes_cnt * 8) in
  let expected = Word.of_int ~width:(bytes_cnt * 8) expected in
  let expected = Word.concat head expected in
  check_gpr init bytes r10 expected arch ctxt

let popcntw arch ctxt =
  let bytes = "\x7c\x44\x02\xf4" in (** popcntw r4, r2 *)
  let r4 = find_gpr arch "R4" in
  let r2 = find_gpr arch "R2" in
  let width = arch_width arch in
  let value,expected = match arch with
    | `ppc ->
      Word.of_int64 ~width 0x10000001L,
      Word.of_int64 ~width 0x000000002L
    | _ ->
      Word.of_int64 0xA0200040_10000001L,
      Word.of_int64 0x400000002L in (** 4 bits set in first word, and 2 in second  *)
  let init = Bil.[
      r2 := int value;
    ] in
  check_gpr init bytes r4 expected arch ctxt

let popcntd ctxt =
  let arch = `ppc64 in
  let bytes = "\x7c\x44\x03\xf4" in (** popcntw r4, r2 *)
  let r4 = find_gpr arch "R4" in
  let r2 = find_gpr arch "R2" in
  let width = arch_width arch in
  let value = Word.of_int64 0x0F00000F_00000001L in
  let expected = Word.of_int ~width 9 in
  let init = Bil.[
      r2 := int value;
    ] in
  check_gpr init bytes r4 expected arch ctxt

let bperm ctxt =
  let arch = `ppc64 in
  let bytes = "\x7c\xa1\x49\xf8" in (** bpermd r1, r5, r9  *)
  let r1 = find_gpr arch "R1" in
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let width = arch_width arch in
  let index_bits = Word.of_int64 0x05_00_01_44_25_1B_02_04L in
  let value = Word.of_int64 0xFFABCDEF0A0B0C0DL in
  let expected = Word.of_int ~width 0xe3 in
  let init = Bil.[
      r5 := int index_bits;
      r9 := int value;
    ] in
  check_gpr init bytes r1 expected arch ctxt

let suite = "logical" >::: [
    "andi.32"     >:: andi_dot `ppc;
    "andis.32"    >:: andis_dot `ppc;
    "and32"       >:: and_ `ppc;
    "and.32"      >:: and_dot `ppc;
    "andc32"      >:: andc `ppc;
    "andc.32"     >:: andc_dot `ppc;
    "ori32"       >:: ori `ppc;
    "oris32"      >:: oris `ppc;
    "or32"        >:: or_ `ppc;
    "or.32"       >:: or_dot `ppc;
    "orc32"       >:: orc `ppc;
    "orc.32"      >:: orc_dot `ppc;
    "xori32"      >:: xori `ppc;
    "xoris23"     >:: xoris `ppc;
    "xor32"       >:: xor_ `ppc;
    "xor.32"      >:: xor_dot `ppc;
    "nand32"      >:: nand `ppc;
    "nand.32"     >:: nand_dot `ppc;
    "nor32"       >:: nor `ppc;
    "nor.32"      >:: nor_dot `ppc;
    "eqv32"       >:: eqv `ppc;
    "eqv.32"      >:: eqv_dot `ppc;
    "extsb32"     >:: extsb `ppc;
    "extsh32"     >:: extsh `ppc;
    "extsw32"     >:: extsw `ppc;
    "exts.32"     >:: exts_dot `ppc;
    "cntlzw32: 1" >:: cntlzw `ppc 0x0 32;
    "cntlzw32: 2" >:: cntlzw `ppc 0x4000000 5;
    "cntlzw32: 3" >:: cntlzw `ppc 0x40000000 1;
    "cntlzw32: 4" >:: cntlzw `ppc 0x80000000 0;
    "cnttzw32: 1" >:: cnttzw `ppc 0x0 32;
    "cnttzw32: 2" >:: cnttzw `ppc 0x1 0;
    "cnttzw32: 3" >:: cnttzw `ppc 0x2 1;
    "cnttzw32: 4" >:: cnttzw `ppc 0x20 5;
    "cmpb32: 0"   >:: cmpb `ppc ~bytes_cnt:3 0x31_41_AD 0x34_42_AD 0x00_00_FF;
    "cmpb32: 1"   >:: cmpb `ppc ~bytes_cnt:3 0x31_42_45 0x34_42_AD 0x00_FF_00;
    "cmpb32: 2"   >:: cmpb `ppc ~bytes_cnt:3 0 0x34_42_AD 0x0;
    "cmpb32: 3"   >:: cmpb `ppc ~bytes_cnt:3 0x34_42_AD 0x34_42_AD 0xFF_FF_FF;
    "popcntw32"   >:: popcntw `ppc;

    "andi.64"     >:: andi_dot `ppc64;
    "andis.64"    >:: andis_dot `ppc64;
    "and64"       >:: and_ `ppc64;
    "and.64"      >:: and_dot `ppc64;
    "andc64"      >:: andc `ppc64;
    "andc.64"     >:: andc_dot `ppc64;
    "ori64"       >:: ori `ppc64;
    "oris64"      >:: oris `ppc64;
    "or64"        >:: or_ `ppc64;
    "or.64"       >:: or_dot `ppc64;
    "orc64"       >:: orc `ppc64;
    "orc.64"      >:: orc_dot `ppc64;
    "xori64"      >:: xori `ppc64;
    "xoris64"     >:: xoris `ppc64;
    "xor64"       >:: xor_ `ppc64;
    "xor.64"      >:: xor_dot `ppc64;
    "nand64"      >:: nand `ppc64;
    "nand.64"     >:: nand_dot `ppc64;
    "nor64"       >:: nor `ppc64;
    "nor.64"      >:: nor_dot `ppc64;
    "eqv64"       >:: eqv `ppc64;
    "eqv.64"      >:: eqv_dot `ppc64;
    "extsb64"     >:: extsb `ppc64;
    "extsh64"     >:: extsh `ppc64;
    "extsw64"     >:: extsw `ppc64;
    "exts.64"     >:: exts_dot `ppc64;
    "cntlz64: 1"  >:: cntlzw `ppc64 0x0 32;
    "cntlz64: 2"  >:: cntlzw `ppc64 0x4000000 5;
    "cntlz64: 3"  >:: cntlzw `ppc64 0x40000000 1;
    "cntlz64: 4"  >:: cntlzw `ppc64 0x80000000 0;
    "cnttz64: 1"  >:: cnttzw `ppc64 0x0 32;
    "cnttz64: 2"  >:: cnttzw `ppc64 0x1 0;
    "cnttz64: 3"  >:: cnttzw `ppc64 0x2 1;
    "cnttz64: 4"  >:: cnttzw `ppc64 0x20 5;
    "cntlzd"      >:: cntlzd;
    "cnttzd"      >:: cnttzd;
    "cmpb64: 1"   >:: cmpb `ppc64 ~bytes_cnt:3 0x31_42_45 0x34_42_AD 0x00_FF_00;
    "cmpb64: 2"   >:: cmpb `ppc64 ~bytes_cnt:3 0 0x34_42_AD 0x0;
    "cmpb64: 3"   >:: cmpb `ppc64 ~bytes_cnt:3 0x34_42_AD 0x34_42_AD 0xFF_FF_FF;
    "popcntw64"   >:: popcntw `ppc64;
    "popcntd"     >:: popcntd;
    "bperm64"     >:: bperm;
  ]
