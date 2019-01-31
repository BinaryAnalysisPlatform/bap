open Core_kernel
open Bap.Std
open OUnit2

open Powerpc_tests_helpers

type expected = LT | GT | EQ

let cr_field_num = 7  (** in IBM style, i.e. least significant byte  *)

let print_bits lt gt eq =
  let str = function
    | None -> "unknown"
    | Some x -> Word.to_string x in
  printf "lt %s; gt %s; eq %s; \n" (str lt) (str gt) (str eq)

let check ctxt expected =
  let cr_lt = cr_bit 28 in
  let cr_gt = cr_bit 29 in
  let cr_eq = cr_bit 30 in
  let lt = lookup_var ctxt cr_lt in
  let gt = lookup_var ctxt cr_gt in
  let eq = lookup_var ctxt cr_eq in
  match expected with
  | LT ->
    is_equal_words Word.b1 lt &&
    is_equal_words Word.b0 gt &&
    is_equal_words Word.b0 eq
  | GT ->
    is_equal_words Word.b0 lt &&
    is_equal_words Word.b1 gt &&
    is_equal_words Word.b0 eq
  | EQ ->
    is_equal_words Word.b0 lt &&
    is_equal_words Word.b0 gt &&
    is_equal_words Word.b1 eq

let cmpi name ~opcode l_field arch reg_value value expected (_ctxt : test_ctxt) =
  let reg_num = 6 in
  let opcode = Word.of_int ~width:6 opcode in
  let cr_field = Word.of_int ~width:3 cr_field_num in
  let l_field = Word.of_int ~width:1 l_field in
  let reg = Word.of_int ~width:5 reg_num in
  let value = Word.of_int64 ~width:16 value in
  let bytes =
    make_bytes [opcode; cr_field; Word.b0; l_field; reg; value] in
  let reg = find_gpr arch (sprintf "R%d" reg_num) in
  let width = arch_width arch in
  let init = Bil.[reg := int @@ Word.of_int64 ~width reg_value] in
  let c = eval init bytes arch in
  assert_bool (sprintf "%s failed" name) @@ check c expected

let cmpwi = cmpi "cmpwi" ~opcode:11 0
let cmplwi = cmpi "cmplwi" ~opcode:10 0
let cmpdi = cmpi "cmpdi" ~opcode:11 1 `ppc64
let cmpldi = cmpi "cmpldi" ~opcode:10 1 `ppc64

let cmp name ~opt_opcode l_field arch reg1_value reg2_value expected (_ctxt : test_ctxt) =
  let reg1_num = 6 in
  let reg2_num = 7 in
  let opcode = Word.of_int ~width:6 31 in
  let cr_field = Word.of_int ~width:3 cr_field_num in
  let l_field = Word.of_int ~width:1 l_field in
  let reg1 = Word.of_int ~width:5 reg1_num in
  let reg2 = Word.of_int ~width:5 reg2_num in
  let opt_opcode = Word.of_int ~width:10 opt_opcode in
  let fin = Word.b0 in
  let bytes =
    make_bytes [opcode; cr_field; Word.b0; l_field; reg1; reg2; opt_opcode; fin] in
  let reg1 = find_gpr arch (sprintf "R%d" reg1_num) in
  let reg2 = find_gpr arch (sprintf "R%d" reg2_num) in
  let width = arch_width arch in
  let init = Bil.[
      reg1 := int @@ Word.of_int64 ~width reg1_value;
      reg2 := int @@ Word.of_int64 ~width reg2_value;
    ] in
  let c = eval init bytes arch in
  assert_bool (sprintf "%s failed" name) @@ check c expected

let cmpw = cmp "cmpw" ~opt_opcode:0 0
let cmplw = cmp "cmplw" ~opt_opcode:32 0
let cmpd = cmp "cmpd" ~opt_opcode:0 1 `ppc64
let cmpld = cmp "cmpld" ~opt_opcode:32 1 `ppc64

let suite = "compare" >::: [

    "cmpwi32: lt"            >:: cmpwi `ppc 42L 44L LT;
    "cmpwi32: gt"            >:: cmpwi `ppc 44L 42L GT;
    "cmpwi32: eq"            >:: cmpwi `ppc 42L 42L EQ;
    "cmpwi32: 32lt"          >:: cmpwi `ppc 0x0BCDEFAB_00000042L 0x44L LT;
    "cmpwi32: lt signed"     >:: cmpwi `ppc (-42L) (-41L) LT;
    "cmpwi32: gt signed"     >:: cmpwi `ppc 44L (-42L) GT;
    "cmpwi32: eq signed"     >:: cmpwi `ppc (-42L) (-42L) EQ;

    "cmpw32: lt"             >:: cmpw `ppc 42L 44L LT;
    "cmpw32: gt"             >:: cmpw `ppc 44L 42L GT;
    "cmpw32: eq"             >:: cmpw `ppc 42L 42L EQ;
    "cmpw32: 32lt"           >:: cmpw `ppc 0x0BCDEFAB_00000042L 0x44L LT;
    "cmpw32: lt signed"      >:: cmpw `ppc (-42L) (-41L) LT;
    "cmpw32: gt signed"      >:: cmpw `ppc 44L (-42L) GT;
    "cmpw32: eq signed"      >:: cmpw `ppc (-42L) (-42L) EQ;

    "cmpw32: lt"             >:: cmpw `ppc 42L 44L LT;
    "cmpw32: gt"             >:: cmpw `ppc 44L 42L GT;
    "cmpw32: eq"             >:: cmpw `ppc 42L 42L EQ;
    "cmpw32: 32lt"           >:: cmpw `ppc 0x0BCDEFAB_00000042L 0x44L LT;
    "cmpw32: lt signed"      >:: cmpw `ppc (-42L) (-41L) LT;
    "cmpw32: gt signed"      >:: cmpw `ppc 44L (-42L) GT;
    "cmpw32: eq signed"      >:: cmpw `ppc (-42L) (-42L) EQ;

    "cmplwi32: lt"            >:: cmplwi `ppc 42L 44L LT;
    "cmplwi32: gt"            >:: cmplwi `ppc 44L 42L GT;
    "cmplwi32: eq"            >:: cmplwi `ppc 42L 42L EQ;
    "cmplwi32: 32lt"          >:: cmplwi `ppc 0x0BCDEFAB_00000042L 0x44L LT;
    "cmplwi32: lt big"        >:: cmplwi `ppc 42L 0xFFFFFFFFL LT;
    "cmplwi32: gt big"        >:: cmplwi `ppc 0xFFFFFFFF_FFFFFFFFL 42L GT;

    "cmplw32: lt"             >:: cmplw `ppc 42L 44L LT;
    "cmplw32: gt"             >:: cmplw `ppc 44L 42L GT;
    "cmplw32: eq"             >:: cmplw `ppc 42L 42L EQ;
    "cmplw32: 32lt"           >:: cmplw `ppc 0x0BCDEFAB_00000042L 0x44L LT;
    "cmplw32: lt big"         >:: cmplw `ppc 42L 0xFFFFFFFFL LT;
    "cmplw32: gt big"         >:: cmplw `ppc 0xFFFFFFFF_FFFFFFFFL 42L GT;

    "cmpwi64: lt"             >:: cmpwi `ppc64 42L 44L LT;
    "cmpwi64: gt"             >:: cmpwi `ppc64 44L 42L GT;
    "cmpwi64: eq"             >:: cmpwi `ppc64 42L 42L EQ;
    "cmpwi64: 64lt"           >:: cmpwi `ppc64 0x0BCDEFAB_00000042L 0x44L LT;
    "cmpwi64: lt signed"      >:: cmpwi `ppc64 (-42L) (-41L) LT;
    "cmpwi64: gt signed"      >:: cmpwi `ppc64 44L (-42L) GT;
    "cmpwi64: eq signed"      >:: cmpwi `ppc64 (-42L) (-42L) EQ;

    "cmpw64: lt"              >:: cmpw `ppc64 42L 44L LT;
    "cmpw64: gt"              >:: cmpw `ppc64 44L 42L GT;
    "cmpw64: eq"              >:: cmpw `ppc64 42L 42L EQ;
    "cmpw64: 64lt"            >:: cmpw `ppc64 0x0BCDEFAB_00000042L 0x44L LT;
    "cmpw64: lt signed"       >:: cmpw `ppc64 (-42L) (-41L) LT;
    "cmpw64: gt signed"       >:: cmpw `ppc64 44L (-42L) GT;
    "cmpw64: eq signed"       >:: cmpw `ppc64 (-42L) (-42L) EQ;

    "cmplwi64: lt"            >:: cmplwi `ppc64 42L 44L LT;
    "cmplwi64: gt"            >:: cmplwi `ppc64 44L 42L GT;
    "cmplwi64: eq"            >:: cmplwi `ppc64 42L 42L EQ;
    "cmplwi64: 64lt"          >:: cmplwi `ppc64 0x0BCDEFAB_00000042L 0x44L LT;
    "cmplwi64: lt big"        >:: cmplwi `ppc64 42L 0xFFFFFFFFL LT;
    "cmplwi64: gt big"        >:: cmplwi `ppc64 0xFFFFFFFF_FFFFFFFFL 42L GT;

    "cmplw64: lt"             >:: cmplw `ppc64 42L 44L LT;
    "cmplw64: gt"             >:: cmplw `ppc64 44L 42L GT;
    "cmplw64: eq"             >:: cmplw `ppc64 42L 42L EQ;
    "cmplw64: 64lt"           >:: cmplw `ppc64 0x0BCDEFAB_00000042L 0x44L LT;
    "cmplw64: lt big"         >:: cmplw `ppc64 42L 0xFFFFFFFFL LT;
    "cmplw64: gt big"         >:: cmplw `ppc64 0xFFFFFFFF_FFFFFFFFL 42L GT;

    "cmpdi: lt"             >:: cmpdi 42L 44L LT;
    "cmpdi: gt"             >:: cmpdi 44L 42L GT;
    "cmpdi: eq"             >:: cmpdi 42L 42L EQ;
    "cmpdi: 64 operand, 1"  >:: cmpdi 0x0BCDEFAB_00000042L 0x44L GT;
    "cmpdi: 64 operand, 2"  >:: cmpdi 0xFBCDEFAB_00000042L 0x44L LT;
    "cmpdi: lt signed"      >:: cmpdi (-42L) (-41L) LT;
    "cmpdi: gt signed"      >:: cmpdi 44L (-42L) GT;
    "cmpdi: eq signed"      >:: cmpdi (-42L) (-42L) EQ;

    "cmpd: lt"              >:: cmpd 42L 44L LT;
    "cmpd: gt"              >:: cmpd 44L 42L GT;
    "cmpd: eq"              >:: cmpd 42L 42L EQ;
    "cmpd: 64 operand, 1"   >:: cmpd 0x0BCDEFAB_00000042L 0x44L GT;
    "cmpd: 64 operand, 2"   >:: cmpd 0xFBCDEFAB_00000042L 0x44L LT;
    "cmpd: lt signed"       >:: cmpd (-42L) (-41L) LT;
    "cmpd: gt signed"       >:: cmpd 44L (-42L) GT;
    "cmpd: eq signed"       >:: cmpd (-42L) (-42L) EQ;

    "cmpldi: lt"            >:: cmpldi 42L 44L LT;
    "cmpldi: gt"            >:: cmpldi 44L 42L GT;
    "cmpldi: eq"            >:: cmpldi 42L 42L EQ;
    "cmpldi: 64 operand, 1" >:: cmpldi 0x0BCDEFAB_00000042L 0x44L GT;
    "cmpldi: 64 operand, 2" >:: cmpldi 0xFBCDEFAB_00000042L 0x44L GT;
    "cmpldi: lt big"        >:: cmpldi 42L 0xFFFFFFFFL LT;
    "cmpldi: gt big"        >:: cmpldi 0xFFFFFFFF_FFFFFFFFL 42L GT;

    "cmpld: lt"             >:: cmpld 42L 44L LT;
    "cmpld: gt"             >:: cmpld 44L 42L GT;
    "cmpld: eq"             >:: cmpld 42L 42L EQ;
    "cmpld: 64 operand, 1"  >:: cmpld 0x0BCDEFAB_00000042L 0x44L GT;
    "cmpld: 64 operand, 2"  >:: cmpld 0xFBCDEFAB_00000042L 0x44L GT;
    "cmpld: lt big"         >:: cmpld 42L 0xFFFFFFFFL LT;
    "cmpld: gt big"         >:: cmpld 0xFFFFFFFF_FFFFFFFFL 42L GT;

  ]
