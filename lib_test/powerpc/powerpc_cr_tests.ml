open Core_kernel.Std
open Bap.Std
open OUnit2

open Powerpc_tests_helpers

let crand arch ctxt =
  let name = "CRAND" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 257; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crnand arch ctxt =
  let name = "CRNAND" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 225; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b0 arch ctxt

let cror arch ctxt =
  let name = "CROR" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 449; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crxor arch ctxt =
  let name = "CRXOR" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 193; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b0 arch ctxt

let crnor arch ctxt =
  let name = "CRNOR" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 33; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b0;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let creqv arch ctxt =
  let name = "CREQV" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 289; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b1;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crandc arch ctxt =
  let name = "CRANDC" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 129; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b1;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let crorc arch ctxt =
  let name = "CRORC" in
  let bytes = make_insn ~name `XL [19; 1; 2; 3; 417; 0] in
  let cr1 = cr_bit 1 in
  let cr2 = cr_bit 2 in
  let cr3 = cr_bit 3 in
  let init = Bil.[
      cr2 := int Word.b0;
      cr3 := int Word.b0;
    ] in
  check_gpr init bytes cr1 Word.b1 arch ctxt

let mcrf arch ctxt =
  let name = "MCRF" in
  let bytes = make_insn ~name `XL [19; 1; 0; 3; 0; 0; 0; 0] in
  let cr4 = cr_bit 4 in
  let cr5 = cr_bit 5 in
  let cr6 = cr_bit 6 in
  let cr7 = cr_bit 7 in
  let cr12 = cr_bit 12 in
  let cr13 = cr_bit 13 in
  let cr14 = cr_bit 14 in
  let cr15 = cr_bit 15 in
  let init = Bil.[
      cr12 := int Word.b1;
      cr13 := int Word.b0;
      cr14 := int Word.b1;
      cr15 := int Word.b0;
    ] in
  let ctxt = eval init bytes arch in
  let cr4_val = lookup_var ctxt cr4 in
  let cr5_val = lookup_var ctxt cr5 in
  let cr6_val = lookup_var ctxt cr6 in
  let cr7_val = lookup_var ctxt cr7 in
  assert_bool "mcrf failed: bit 4" (is_equal_words Word.b1 cr4_val);
  assert_bool "mcrf failed: bit 5" (is_equal_words Word.b0 cr5_val);
  assert_bool "mcrf failed: bit 6" (is_equal_words Word.b1 cr6_val);
  assert_bool "mcrf failed: bit 7" (is_equal_words Word.b0 cr7_val)

let suite  = "CR" >::: [
    "crand"   >:: crand `ppc;
    "crnand"  >:: crnand `ppc;
    "cror"    >:: cror `ppc;
    "crxor"   >:: crxor `ppc;
    "crnor"   >:: crnor `ppc;
    "creqv"   >:: creqv `ppc;
    "crandc"  >:: crandc `ppc;
    "crorc"   >:: crorc `ppc;
    "mcrf"    >:: mcrf `ppc;

    "crand 64"   >:: crand `ppc64;
    "crnand 64"  >:: crnand `ppc64;
    "cror 64"    >:: cror `ppc64;
    "crxor 64"   >:: crxor `ppc64;
    "crnor 64"   >:: crnor `ppc64;
    "creqv 64"   >:: creqv `ppc64;
    "crandc 64"  >:: crandc `ppc64;
    "crorc 64"   >:: crorc `ppc64;
    "mcrf 64"    >:: mcrf `ppc64;
  ]
