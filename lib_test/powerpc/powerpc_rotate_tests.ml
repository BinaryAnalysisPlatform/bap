open Core_kernel
open OUnit2
open Bap.Std

open Powerpc_tests_helpers

let circ_shift32 x sh =
  if sh = 0 then x
  else
    let width = Word.bitwidth x in
    let x = Word.extract_exn ~hi:31 x in
    let x = Word.concat x x in
    let bits = Word.bitwidth x in
    let right = Word.extract_exn ~lo:(bits - sh) x in
    Word.extract_exn ~hi:(width - 1) (Word.concat x right)

let mask64 start stop =
  let make f w =
    if w > 0 then Some (f w)
    else None in
  let (^) = Word.concat in
  let concat x y z =
    match List.filter_map ~f:ident [x;y;z;] with
    | [x] -> x
    | [x;y] -> x ^ y
    | [x;y;z;] -> x ^ y ^ z
    | _ -> failwith "list is empty" in
  let width = 64 in
  if start <= stop then
    let a1 = make Word.zero start in
    let a2 = make Word.ones (stop - start + 1) in
    let a3 = make Word.zero (width - stop - 1) in
    concat a1 a2 a3
  else
    let a1 = make Word.ones stop in
    let a2 = make Word.zero (start - stop + 1) in
    let a3 = make Word.ones (width - start - 1) in
    concat a1 a2 a3

let mask32 width start stop =
  Word.extract_exn ~hi:(width - 1) @@
  mask64 (start + 32) (stop + 32)

let circ_shift64 x sh =
  if sh = 0 then x
  else
    let bits = Word.bitwidth x in
    let right = Word.extract_exn ~lo:(bits - sh) x in
    Word.extract_exn ~hi:(bits - 1) (Word.concat x right)

let rlwinm arch ~sh ~from ~to_ ctxt =
  let name = "RLWINM" in
  let bytes = make_insn ~name `M [21; 9; 10; sh; from; to_; 0] in
  let width = arch_width arch in
  let x = Word.of_int64 ~width 0xAABBCCDDL in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9 := int x;
    ] in
  let shifted = circ_shift32 x sh in
  let mask = mask32 width from to_ in
  let expected = Word.(shifted land mask) in
  check_gpr init bytes r10 expected arch ctxt

let rlwnm arch ~sh ~from ~to_ ctxt =
  let name = "RLWNM" in
  let bytes = make_insn ~name `M [23; 9; 10; 3; from; to_; 0] in
  let width = arch_width arch in
  let x = Word.of_int64 ~width 0xAABBCCDDL in
  let shw = Word.of_int ~width sh in
  let r3 = find_gpr arch "R3" in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r3 := int shw;
      r9 := int x;
  ] in
  let shifted = circ_shift32 x sh in
  let mask = mask32 width from to_ in
  let expected = Word.(shifted land mask) in
  check_gpr init bytes r10 expected arch ctxt

let rlwimi arch ~sh ~from ~to_ ctxt =
  let name = "RLWIMI" in
  let bytes = make_insn ~name `M [20; 9; 10; sh; from; to_; 0] in
  let width = arch_width arch in
  let x = Word.of_int64 ~width 0xAABBCCDDL in
  let y = Word.of_int64 ~width 0xAABBCCDD_FFEEAA42L in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9  := int x;
      r10 := int y;
  ] in
  let shifted = circ_shift32 x sh in
  let mask = mask32 width from to_ in
  let expected = Word.((shifted land mask) lor (y land (lnot mask))) in
  check_gpr init bytes r10 expected arch ctxt

let extract_from_int hi lo v =
  let w = Word.of_int ~width:32 v in
  let w'= Word.extract_exn ~hi ~lo w in
  Word.to_int_exn w'

let swap_msb x =
  let x = Word.of_int ~width:32 x in
  let b1 = Word.extract_exn ~hi:5 ~lo:5 x in
  let b2 = Word.extract_exn ~hi:4 x in
  let y = Word.concat b2 b1 in
  Word.to_int_exn y

let rldicl ~sh ~from ctxt =
  let arch = `ppc64 in
  let name = "RLDICL" in
  let sh0_4 = extract_from_int 4 0 sh in
  let sh5 = extract_from_int 5 5 sh in
  let from' = swap_msb from in
  let bytes = make_insn ~name `MD [30; 9; 10; sh0_4; from'; 0; sh5; 0] in
  let x = Word.of_int64 0xAABBCCDD_4242FFAAL in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9 := int x;
    ] in
  let shifted = circ_shift64 x sh in
  let mask = mask64 from 63  in
  let expected = Word.(shifted land mask) in
  check_gpr init bytes r10 expected arch ctxt

let rldicr ~sh ~to_ ctxt =
  let arch = `ppc64 in
  let name = "RLDICR" in
  let sh0_4 = extract_from_int 4 0 sh in
  let sh5 = extract_from_int 5 5 sh in
  let to' = swap_msb to_ in
  let bytes = make_insn ~name `MD [30; 9; 10; sh0_4; to'; 1; sh5; 0] in
  let x = Word.of_int64 0xAABBCCDD_4242FFAAL in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9 := int x;
    ] in
  let shifted = circ_shift64 x sh in
  let mask = mask64 0 to_  in
  let expected = Word.(shifted land mask) in
  check_gpr init bytes r10 expected arch ctxt

let rldic ~sh ~from ctxt =
  let arch = `ppc64 in
  let name = "RLDIC" in
  let sh0_4 = extract_from_int 4 0 sh in
  let sh5 = extract_from_int 5 5 sh in
  let from' = swap_msb from in
  let bytes = make_insn ~name `MD [30; 9; 10; sh0_4; from'; 2; sh5; 0] in
  let x = Word.of_int64 0xAABBCCDD_4242FFAAL in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9 := int x;
    ] in
  let shifted = circ_shift64 x sh in
  let mask = mask64 from (63 - sh)  in
  let expected = Word.(shifted land mask) in
  check_gpr init bytes r10 expected arch ctxt

let rldcl ~sh ~from ctxt =
  let arch = `ppc64 in
  let name = "RLDCL" in
  let from' = swap_msb from in
  let bytes = make_insn ~name `MDS [30; 9; 10; 11; from'; 8; 0] in
  let shw = Word.of_int ~width:64 sh in
  let x = Word.of_int64 0xAABBCCDD_4242FFAAL in
  let r9  = find_gpr arch "R9" in
  let r11 = find_gpr arch "R11" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  let shifted = circ_shift64 x sh in
  let mask = mask64 from 63  in
  let expected = Word.(shifted land mask) in
  check_gpr init bytes r10 expected arch ctxt

let rldcr ~sh ~to_ ctxt =
  let arch = `ppc64 in
  let name = "RLDCR" in
  let to' = swap_msb to_ in
  let bytes = make_insn ~name `MDS [30; 9; 10; 11; to'; 9; 0] in
  let x = Word.of_int64 0xAABBCCDD_4242FFAAL in
  let shw = Word.of_int ~width:64 sh in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r11 = find_gpr arch "R11" in
  let init = Bil.[
      r9 := int x;
      r11 := int shw;
    ] in
  let shifted = circ_shift64 x sh in
  let mask = mask64 0 to_  in
  let expected = Word.(shifted land mask) in
  check_gpr init bytes r10 expected arch ctxt

let rldimi ~sh ~from ctxt =
  let arch = `ppc64 in
  let name = "RLDIMI" in
  let sh0_4 = extract_from_int 4 0 sh in
  let sh5 = extract_from_int 5 5 sh in
  let from' = swap_msb from in
  let bytes = make_insn ~name `MD [30; 9; 10; sh0_4; from'; 3; sh5; 0] in
  let x = Word.of_int64 0xAABBCCDD_4242FFAAL in
  let y = Word.of_int64 0xFFABCDEF_FFEEAA42L in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let init = Bil.[
      r9  := int x;
      r10 := int y;
    ] in
  let shifted = circ_shift64 x sh in
  let mask = mask64 from (63 - sh) in
  let expected = Word.((shifted land mask) lor (y land (lnot mask))) in
  check_gpr init bytes r10 expected arch ctxt

let suite = "rotate" >::: [
    "rlwinm sh:3 from:5 to:10"  >:: rlwinm `ppc ~sh:3 ~from:5 ~to_:10;
    "rlwinm sh:3 from:10 to:5"  >:: rlwinm `ppc ~sh:3 ~from:10 ~to_:5;
    "rlwinm sh:3 from:0 to:5"   >:: rlwinm `ppc ~sh:3 ~from:0 ~to_:5;
    "rlwinm sh:3 from:5 to:0"   >:: rlwinm `ppc ~sh:3 ~from:5 ~to_:0;
    "rlwinm sh:3 from:0 to:0"   >:: rlwinm `ppc ~sh:3 ~from:0 ~to_:0;
    "rlwinm sh:3 from:0 to:0"   >:: rlwinm `ppc ~sh:3 ~from:0 ~to_:0;
    "rlwinm sh:0 from:3 to:5"   >:: rlwinm `ppc ~sh:0 ~from:3 ~to_:5;

    "rlwnm sh:3 from:5 to:10"   >:: rlwnm `ppc ~sh:3 ~from:5 ~to_:10;
    "rlwnm sh:3 from:10 to:5"   >:: rlwnm `ppc ~sh:3 ~from:10 ~to_:5;
    "rlwnm sh:3 from:0 to:5"    >:: rlwnm `ppc ~sh:3 ~from:0 ~to_:5;
    "rlwnm sh:3 from:5 to:0"    >:: rlwnm `ppc ~sh:3 ~from:5 ~to_:0;
    "rlwnm sh:3 from:0 to:0"    >:: rlwnm `ppc ~sh:3 ~from:0 ~to_:0;
    "rlwnm sh:3 from:0 to:0"    >:: rlwnm `ppc ~sh:3 ~from:0 ~to_:0;
    "rlwnm sh:0 from:3 to:5"    >:: rlwnm `ppc ~sh:0 ~from:3 ~to_:5;

    "rlwimi sh:3 from:5 to:10"  >:: rlwimi `ppc ~sh:3 ~from:5 ~to_:10;
    "rlwimi sh:3 from:10 to:5"  >:: rlwimi `ppc ~sh:3 ~from:10 ~to_:5;
    "rlwimi sh:3 from:0 to:5"   >:: rlwimi `ppc ~sh:3 ~from:0 ~to_:5;
    "rlwimi sh:3 from:5 to:0"   >:: rlwimi `ppc ~sh:3 ~from:5 ~to_:0;
    "rlwimi sh:3 from:0 to:0"   >:: rlwimi `ppc ~sh:3 ~from:0 ~to_:0;
    "rlwimi sh:3 from:0 to:0"   >:: rlwimi `ppc ~sh:3 ~from:0 ~to_:0;
    "rlwimi sh:0 from:3 to:5"   >:: rlwimi `ppc ~sh:0 ~from:3 ~to_:5;

    "rlwinm ppc64 sh:3 from:5 to:10"  >:: rlwinm `ppc64 ~sh:3 ~from:5 ~to_:10;
    "rlwinm ppc64 sh:3 from:10 to:5"  >:: rlwinm `ppc64 ~sh:3 ~from:10 ~to_:5;
    "rlwinm ppc64 sh:3 from:0 to:5"   >:: rlwinm `ppc64 ~sh:3 ~from:0 ~to_:5;
    "rlwinm ppc64 sh:3 from:5 to:0"   >:: rlwinm `ppc64 ~sh:3 ~from:5 ~to_:0;
    "rlwinm ppc64 sh:3 from:0 to:0"   >:: rlwinm `ppc64 ~sh:3 ~from:0 ~to_:0;
    "rlwinm ppc64 sh:3 from:0 to:0"   >:: rlwinm `ppc64 ~sh:3 ~from:0 ~to_:0;
    "rlwinm ppc64 sh:0 from:3 to:5"   >:: rlwinm `ppc64 ~sh:0 ~from:3 ~to_:5;

    "rlwnm ppc64 sh:3 from:5 to:10"   >:: rlwnm `ppc64 ~sh:3 ~from:5 ~to_:10;
    "rlwnm ppc64 sh:3 from:10 to:5"   >:: rlwnm `ppc64 ~sh:3 ~from:10 ~to_:5;
    "rlwnm ppc64 sh:3 from:0 to:5"    >:: rlwnm `ppc64 ~sh:3 ~from:0 ~to_:5;
    "rlwnm ppc64 sh:3 from:5 to:0"    >:: rlwnm `ppc64 ~sh:3 ~from:5 ~to_:0;
    "rlwnm ppc64 sh:3 from:0 to:0"    >:: rlwnm `ppc64 ~sh:3 ~from:0 ~to_:0;
    "rlwnm ppc64 sh:3 from:0 to:0"    >:: rlwnm `ppc64 ~sh:3 ~from:0 ~to_:0;
    "rlwnm ppc64 sh:0 from:3 to:5"    >:: rlwnm `ppc64 ~sh:0 ~from:3 ~to_:5;

    "rlwimi ppc64 sh:3 from:5 to:10"  >:: rlwimi `ppc64 ~sh:3 ~from:5 ~to_:10;
    "rlwimi ppc64 sh:3 from:10 to:5"  >:: rlwimi `ppc64 ~sh:3 ~from:10 ~to_:5;
    "rlwimi ppc64 sh:3 from:0 to:5"   >:: rlwimi `ppc64 ~sh:3 ~from:0 ~to_:5;
    "rlwimi ppc64 sh:3 from:5 to:0"   >:: rlwimi `ppc64 ~sh:3 ~from:5 ~to_:0;
    "rlwimi ppc64 sh:3 from:0 to:0"   >:: rlwimi `ppc64 ~sh:3 ~from:0 ~to_:0;
    "rlwimi ppc64 sh:3 from:0 to:0"   >:: rlwimi `ppc64 ~sh:3 ~from:0 ~to_:0;
    "rlwimi ppc64 sh:0 from:3 to:5"   >:: rlwimi `ppc64 ~sh:0 ~from:3 ~to_:5;

    "rldicl ppc64 sh:4 from:25"       >:: rldicl ~sh:4 ~from:25;
    "rldicl ppc64 sh:4 from:55"       >:: rldicl ~sh:4 ~from:55;
    "rldicl ppc64 sh:4 from:0"        >:: rldicl ~sh:4 ~from:0;
    "rldicl ppc64 sh:0 from:25"       >:: rldicl ~sh:0 ~from:25;
    "rldicl ppc64 sh:0 from:0"        >:: rldicl ~sh:0 ~from:0;

    "rldicr ppc64 sh:4 to:25"         >:: rldicr ~sh:4 ~to_:25;
    "rldicr ppc64 sh:4 to:55"         >:: rldicr ~sh:4 ~to_:55;
    "rldicr ppc64 sh:4 to:0"          >:: rldicr ~sh:4 ~to_:0;
    "rldicr ppc64 sh:0 to:25"         >:: rldicr ~sh:0 ~to_:25;
    "rldicr ppc64 sh:0 to:0"          >:: rldicr ~sh:0 ~to_:0;

    "rldic ppc64 sh:4 from:25"        >:: rldic ~sh:4 ~from:25;
    "rldic ppc64 sh:4 from:55"        >:: rldic ~sh:4 ~from:55;
    "rldic ppc64 sh:4 from:0"         >:: rldic ~sh:4 ~from:0;
    "rldic ppc64 sh:0 from:25"        >:: rldic ~sh:0 ~from:25;
    "rldic ppc64 sh:0 from:0"         >:: rldic ~sh:0 ~from:0;

    "rldcl ppc64 sh:4 from:25"        >:: rldcl ~sh:4 ~from:25;
    "rldcl ppc64 sh:4 from:55"        >:: rldcl ~sh:4 ~from:55;
    "rldcl ppc64 sh:4 from:0"         >:: rldcl ~sh:4 ~from:0;
    "rldcl ppc64 sh:0 from:25"        >:: rldcl ~sh:0 ~from:25;
    "rldcl ppc64 sh:0 from:0"         >:: rldcl ~sh:0 ~from:0;

    "rldcr ppc64 sh:4 to:25"          >:: rldcr ~sh:4 ~to_:25;
    "rldcr ppc64 sh:4 to:55"          >:: rldcr ~sh:4 ~to_:55;
    "rldcr ppc64 sh:4 to:0"           >:: rldcr ~sh:4 ~to_:0;
    "rldcr ppc64 sh:0 to:25"          >:: rldcr ~sh:0 ~to_:25;
    "rldcr ppc64 sh:0 to:0"           >:: rldcr ~sh:0 ~to_:0;

    "rldimi ppc64 sh:4 from:25"       >:: rldimi ~sh:4 ~from:25;
    "rldimi ppc64 sh:4 from:55"       >:: rldimi ~sh:4 ~from:55;
    "rldimi ppc64 sh:4 from:0"        >:: rldimi ~sh:4 ~from:0;
    "rldimi ppc64 sh:0 from:25"       >:: rldimi ~sh:0 ~from:25;
    "rldimi ppc64 sh:0 from:0"        >:: rldimi ~sh:0 ~from:0;


  ]
