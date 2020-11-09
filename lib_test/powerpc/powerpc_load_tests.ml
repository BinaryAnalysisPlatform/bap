open Core_kernel
open Bap.Std
open OUnit2

open Powerpc_tests_helpers

let endian = BigEndian

let env_of_arch arch =
  mem arch, Size.in_bits (Arch.addr_size arch)

let lz name opcode size arch ~d_addr ~value ctxt =
  let bytes = make_insn ~name `D [opcode; 9; 28; d_addr] in
  let mem, width = env_of_arch arch in
  let r9 = find_gpr arch "R9" in
  let r28 = find_gpr arch "R28" in
  let addr = 0xABCDEF00 in
  let ea = Word.of_int ~width (addr + d_addr) in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian size;
      r28 := int (Word.of_int ~width addr);
      r9 := int (Word.of_int64 ~width 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width value in
  check_gpr init bytes r9 expected arch ctxt

let lbz = lz "LBZ" 34 `r8
let lhz = lz "LHZ" 40 `r16
let lwz = lz "LWZ" 32 `r32

let lz_zero_reg name opcode size arch ~d_addr ~value ctxt =
  let bytes = make_insn ~name `D [opcode; 9; 0; d_addr] in
  let mem, width = env_of_arch arch in
  let r9 = find_gpr arch "R9" in
  let ea = Word.of_int ~width d_addr in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian size;
      r9 := int (Word.of_int64 ~width 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width value in
  check_gpr init bytes r9 expected arch ctxt

let lbz_zero_reg = lz_zero_reg "LBZ" 34 `r8
let lhz_zero_reg = lz_zero_reg "LHZ" 40 `r16
let lwz_zero_reg = lz_zero_reg "LWZ" 32 `r32

let lzx name opt size arch ~value ctxt =
  let bytes = make_insn ~name `X [31; 9; 10; 29; opt] in
  let mem, width = env_of_arch arch in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r29 = find_gpr arch "R29" in
  let x = 0xABCD0000 in
  let y = 0x0000FF42 in
  let ea = Word.of_int ~width (x + y) in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian size;
      r29 := int @@ Word.of_int ~width x;
      r10 := int @@ Word.of_int ~width y;
      r9 := int (Word.of_int64 ~width 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width value in
  check_gpr init bytes r9 expected arch ctxt

let lbzx = lzx "LBZX" 87 `r8
let lhzx = lzx "LHZX" 279 `r16
let lwzx = lzx "LWZX" 23 `r32

let lzx_zero_reg name opt size arch ~value ctxt =
  let bytes = make_insn ~name `X [31; 9; 0; 10; opt] in
  let mem, width = env_of_arch arch in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let addr = 0xABCD0000 in
  let ea = Word.of_int ~width addr in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian size;
      r10 := int @@ Word.of_int ~width addr;
      r9 := int (Word.of_int64 ~width 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width value in
  check_gpr init bytes r9 expected arch ctxt

let lbzx_zero_reg = lzx_zero_reg "LBZX" 87 `r8
let lhzx_zero_reg = lzx_zero_reg "LHZX" 279 `r16
let lwzx_zero_reg = lzx_zero_reg "LWZX" 23 `r32

let lzu name opcode size arch ~d_addr ~value ctxt =
  let bytes = make_insn ~name `D [opcode; 9; 28; d_addr] in
  let mem, width = env_of_arch arch in
  let r9 = find_gpr arch "R9" in
  let r28 = find_gpr arch "R28" in
  let addr = 0xABCDEF04 in
  let ea = Word.of_int ~width (addr + d_addr) in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian size;
      r28 := int (Word.of_int ~width addr);
      r9 := int (Word.of_int64 ~width 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width value in
  check_gpr init bytes r9 expected arch ctxt;
  let expected_addr = Word.of_int ~width (addr + d_addr) in
  check_gpr init bytes r28 expected_addr arch ctxt

let lbzu = lzu "LBZU" 35 `r8
let lhzu = lzu "LHZU" 41 `r16
let lwzu = lzu "LWZU" 33 `r32

let lzux name opt_opcode size arch ~value ctxt =
  let bytes = make_insn ~name `X [31; 9; 29; 10; opt_opcode] in
  let mem, width = env_of_arch arch in
  let r9 = find_gpr arch "R9" in
  let r10 = find_gpr arch "R10" in
  let r29 = find_gpr arch "R29" in
  let x = 0xABCD0000 in
  let y = 0x0000EF42 in
  let ea = Word.of_int ~width (x + y) in
  let x = Word.of_int ~width x in
  let y = Word.of_int ~width y in
  let data = Word.of_int ~width:32 value in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian size;
      r29 := int x;
      r10 := int y;
      r9 := int (Word.of_int64 ~width 0xFFFFFFFFFFFFFFFFL);
    ] in
  let expected = Word.of_int ~width value in
  check_gpr init bytes r9 expected arch ctxt;
  let expected_addr = Word.of_int ~width 0xABCDEF42 in
  check_gpr init bytes r29 expected_addr arch ctxt

let lbzux = lzux "LBZUX" 119 `r8
let lhzux = lzux "LHZUX" 311 `r16
let lwzux = lzux "LWZUX" 55 `r32

let lha arch ctxt =
  let bytes = "\xa8\x29\x00\x05" in (** lha r1, 5(r9)  *)
  let r1 = find_gpr arch "R1" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let ea = Word.of_int ~width (addr + 5) in
  let data = Word.of_int ~width 0xffab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r9 := int (Word.of_int ~width addr);
    ] in
  let expected = Word.of_int64 ~width 0xffffffff_ffffffabL in
  check_gpr init bytes r1 expected arch ctxt

let lhax arch ctxt =
  let bytes = "\x7c\x25\x4a\xae" in (** lhax r1, r5, r9 *)
  let r1 = find_gpr arch "R1" in
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int ~width 0xffab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r5 := int (Word.of_int ~width disp);
      r9 := int (Word.of_int ~width addr);
    ] in
  let expected = Word.of_int64 ~width 0xffffffff_ffffffabL in
  check_gpr init bytes r1 expected arch ctxt

let lhau arch ctxt =
  let bytes = "\xac\x29\x00\x05" in (** lhau r1, 5(r9)  *)
  let r1 = find_gpr arch "R1" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let ea = Word.of_int ~width (addr + 5) in
  let data = Word.of_int ~width 0xffab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r9 := int (Word.of_int ~width addr);
    ] in
  let expected = Word.of_int64 ~width 0xffffffff_ffffffabL in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width (addr + 5) in
  check_gpr init bytes r9 expected_addr arch ctxt

let lhaux arch ctxt =
  let bytes = "\x7c\x25\x4a\xee" in (** lhaux r1, r5, r9 *)
  let r1 = find_gpr arch "R1" in
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int ~width 0x0fab in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r16;
      r5 := int (Word.of_int ~width disp);
      r9 := int (Word.of_int ~width addr);
    ] in
  let expected = Word.of_int64 ~width 0x00000000_000000fabL in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width (addr + disp) in
  check_gpr init bytes r5 expected_addr arch ctxt

let lwa arch ctxt =
  let bytes = "\xeb\xeb\x01\x16" in (** lwa r31, 276(r11)  *)
  let r31 = find_gpr arch "R31" in
  let r11 = find_gpr arch "R11" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 276 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int64 ~width 0xffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r11 := int (Word.of_int ~width addr);
    ] in
  let expected = Word.of_int64 ~width 0xffffffff_ffacffabL in
  check_gpr init bytes r31 expected arch ctxt

let lwax arch ctxt =
  let bytes = "\x7c\x25\x4a\xaa" in (** lwax r1, r5, r9 *)
  let r1 = find_gpr arch "R1" in
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int64 ~width 0x0facffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r5 := int (Word.of_int ~width disp);
      r9 := int (Word.of_int ~width addr);
    ] in
  let expected = Word.of_int64 ~width 0x00000000_0facffabL in
  check_gpr init bytes r1 expected arch ctxt

let lwaux arch ctxt =
  let bytes = "\x7c\x25\x4a\xea" in (** lwaux r1, r5, r9 *)
  let r1 = find_gpr arch "R1" in
  let r5 = find_gpr arch "R5" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int64 ~width 0xffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r32;
      r5 := int (Word.of_int ~width disp);
      r9 := int (Word.of_int ~width addr);
    ] in
  let expected = Word.of_int64 ~width 0xffffffff_ffacffabL in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width (addr + disp) in
  check_gpr init bytes r5 expected_addr arch ctxt

let ld ctxt =
  let bytes = "\xe8\x29\x00\x08" in  (** ld r1, 8(r9) *)
  let arch = `ppc64 in
  let r1 = find_gpr arch "R1" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 8 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt

let ldx ctxt =
  let bytes = "\x7c\x28\x48\x2a" in  (** ldx r1, r8, r9 *)
  let arch = `ppc64 in
  let r1 = find_gpr arch "R1" in
  let r8 = find_gpr arch "R8" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
      r8 := int (Word.of_int ~width:64 disp);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt

let ldu ctxt =
  let bytes = "\xe8\x29\x00\x09" in  (** ldu r1, 8(r9) *)
  let arch = `ppc64 in
  let r1 = find_gpr arch "R1" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 8 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + disp) in
  check_gpr init bytes r9 expected_addr arch ctxt

let ldux ctxt =
  let bytes = "\x7c\x28\x48\x6a" in  (** ldux r1, r8, r9  *)
  let arch = `ppc64 in
  let r1 = find_gpr arch "R1" in
  let r8 = find_gpr arch "R8" in
  let r9 = find_gpr arch "R9" in
  let mem, width = env_of_arch arch in
  let addr = 0x00ABCDEF in
  let disp = 5 in
  let ea = Word.of_int ~width (addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int (Word.of_int ~width:64 addr);
      r8 := int (Word.of_int ~width:64 disp);
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = Word.of_int ~width:64 (addr + disp) in
  check_gpr init bytes r8 expected_addr arch ctxt

let ldux_big_addr ctxt =
  let bytes = "\x7c\x28\x48\x6a" in  (** ldux r1, r8, r9  *)
  let arch = `ppc64 in
  let r1 = find_gpr arch "R1" in
  let r8 = find_gpr arch "R8" in
  let r9 = find_gpr arch "R9" in
  let mem, _width = env_of_arch arch in
  let addr = Word.of_int64 0xCDEF4234_CDABCDEFL in
  let disp = Word.of_int64 5L in
  let ea = Word.(addr + disp) in
  let data = Word.of_int64 0xffacbbdd_ffacffabL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian `r64;
      r9 := int addr;
      r8 := int disp;
    ] in
  let expected = data in
  check_gpr init bytes r1 expected arch ctxt;
  let expected_addr = ea in
  check_gpr init bytes r8 expected_addr arch ctxt

let lbrx name opt_opcode size arch ctxt =
  let bytes = make_insn ~name `X [31; 1; 2; 3; opt_opcode; 0] in
  let mem, width = env_of_arch arch in
  let r1 = find_gpr arch "R1" in
  let r2 = find_gpr arch "R2" in
  let r3 = find_gpr arch "R3" in
  let addr = 0xABCDEF00 in
  let d_addr = 0x42 in
  let ea = Word.of_int ~width (addr + d_addr) in
  let data = Word.of_int64 0xABCDEFAB_CCDDEEFFL in
  let init = Bil.[
      mem := store ~mem:(var mem) ~addr:(int ea) (int data) endian size;
      r2 := int (Word.of_int ~width addr);
      r3 := int (Word.of_int ~width d_addr);
    ] in
  let bits = Size.in_bits size in
  let data = Word.extract_exn ~hi:(bits - 1) data in
  let expected =
    Seq.to_list_rev (Word.enum_bytes data endian) |>
    List.fold ~init:None ~f:(fun acc b ->
        match acc with
        | None -> Some b
        | Some p -> Some (Word.concat p b)) |> function
    | None -> assert false
    | Some x -> x in
  let expected = Word.extract_exn ~hi:(width -1) expected in
  check_gpr init bytes r1 expected arch ctxt

let lhbrx = lbrx "LHBRX" 790 `r16
let lwbrx = lbrx "LWBRX" 534 `r32
let ldbrx = lbrx "LDBRX" 532 `r64 `ppc64

let suite = "load" >::: [
    "lbz32 +imm"     >:: lbz `ppc ~d_addr:20 ~value:0x42;
    "lbz32 -imm"     >:: lbz `ppc ~d_addr:(-16) ~value:0x42;
    "lbz32 0 reg"    >:: lbz_zero_reg `ppc ~d_addr:20 ~value:0x42;
    "lhz32 +imm"     >:: lhz `ppc ~d_addr:20 ~value:0x4242;
    "lhz32 -imm"     >:: lhz `ppc ~d_addr:(-16) ~value:0x4252;
    "lhz32 0 reg"    >:: lhz_zero_reg `ppc ~d_addr:20 ~value:0x4242;
    "lwz32 +imm"     >:: lwz `ppc ~d_addr:20 ~value:0xAAAA4242;
    "lwz32 -imm"     >:: lwz `ppc ~d_addr:(-16) ~value:0xAAAA4252;
    "lwz32 0 reg"    >:: lwz_zero_reg `ppc ~d_addr:20 ~value:0xAAAA4242;

    "lbzx32"         >:: lbzx `ppc ~value:0x42;
    "lbzx32 0 reg"   >:: lbzx_zero_reg `ppc ~value:0x42;
    "lhzx32 +imm"    >:: lhzx `ppc ~value:0x4242;
    "lhzx32 0 reg"   >:: lhzx_zero_reg `ppc ~value:0x4242;
    "lwzx32 +imm"    >:: lwzx `ppc ~value:0xAAAA4242;
    "lwzx32 0 reg"   >:: lwzx_zero_reg `ppc ~value:0xAAAA4242;

    "lbzu32 +imm"    >:: lbzu `ppc ~d_addr:20 ~value:0x42;
    "lbzu32 -imm"    >:: lbzu `ppc ~d_addr:(-16) ~value:0x42;
    "lhzu32 +imm"    >:: lhzu `ppc ~d_addr:20 ~value:0x4242;
    "lhzu32 -imm"    >:: lhzu `ppc ~d_addr:(-16) ~value:0x4252;
    "lwzu32 +imm"    >:: lwzu `ppc ~d_addr:20 ~value:0xAAAA4242;
    "lwzu32 -imm"    >:: lwzu `ppc ~d_addr:(-16) ~value:0xAAAA4252;

    "lbzux32"        >:: lbzux `ppc ~value:0x42;
    "lhzux32"        >:: lhzux `ppc ~value:0x4242;
    "lwzux32"        >:: lwzux `ppc ~value:0xAAAA4242;

    "lha32"          >:: lha `ppc;
    "lhax32"         >:: lhax `ppc;
    "lhau32"         >:: lhau `ppc;
    "lhaux32"        >:: lhaux `ppc;
    "lwa32"          >:: lwa `ppc;
    "lwax32"         >:: lwax `ppc;
    "lwaux32"        >:: lwaux `ppc;

    "lhbrx32"        >:: lhbrx `ppc;
    "lwbrx32"        >:: lwbrx `ppc;

    "lbz64 +imm"     >:: lbz `ppc64 ~d_addr:20 ~value:0x42;
    "lbz64 -imm"     >:: lbz `ppc64 ~d_addr:(-16) ~value:0x42;
    "lbz64 0 reg"    >:: lbz_zero_reg `ppc64 ~d_addr:20 ~value:0x42;
    "lhz64 +imm"     >:: lhz `ppc64 ~d_addr:20 ~value:0x4242;
    "lhz64 -imm"     >:: lhz `ppc64 ~d_addr:(-16) ~value:0x4252;
    "lhz64 0 reg"    >:: lhz_zero_reg `ppc64 ~d_addr:20 ~value:0x4242;
    "lwz64 +imm"     >:: lwz `ppc64 ~d_addr:20 ~value:0xAAAA4242;
    "lwz64 -imm"     >:: lwz `ppc64 ~d_addr:(-16) ~value:0xAAAA4252;
    "lwz64 0 reg"    >:: lwz_zero_reg `ppc64 ~d_addr:20 ~value:0xAAAA4242;

    "lbzx64"         >:: lbzx `ppc64 ~value:0x42;
    "lbzx64 0 reg"   >:: lbzx_zero_reg `ppc64 ~value:0x42;
    "lhzx64 +imm"    >:: lhzx `ppc64 ~value:0x4242;
    "lhzx64 0 reg"   >:: lhzx_zero_reg `ppc64 ~value:0x4242;
    "lwzx64 +imm"    >:: lwzx `ppc64 ~value:0xAAAA4242;
    "lwzx64 0 reg"   >:: lwzx_zero_reg `ppc64 ~value:0xAAAA4242;

    "lbzu64 +imm"    >:: lbzu `ppc64 ~d_addr:20 ~value:0x42;
    "lbzu64 -imm"    >:: lbzu `ppc64 ~d_addr:(-16) ~value:0x42;
    "lhzu64 +imm"    >:: lhzu `ppc64 ~d_addr:20 ~value:0x4242;
    "lhzu64 -imm"    >:: lhzu `ppc64 ~d_addr:(-16) ~value:0x4252;
    "lwzu64 +imm"    >:: lwzu `ppc64 ~d_addr:20 ~value:0xAAAA4242;
    "lwzu64 -imm"    >:: lwzu `ppc64 ~d_addr:(-16) ~value:0xAAAA4252;

    "lbzux64"        >:: lbzux `ppc64 ~value:0x42;
    "lhzux64"        >:: lhzux `ppc64 ~value:0x4242;
    "lwzux64"        >:: lwzux `ppc64 ~value:0xAAAA4242;

    "lha64"          >:: lha `ppc64;
    "lhax64"         >:: lhax `ppc64;
    "lhau64"         >:: lhau `ppc64;
    "lhaux64"        >:: lhaux `ppc64;
    "lwa64"          >:: lwa `ppc64;
    "lwax64"         >:: lwax `ppc64;
    "lwaux64"        >:: lwaux `ppc64;
    "ld64"           >:: ld;
    "ldx64"          >:: ldx;
    "ldu64"          >:: ldu;
    "ldux64"         >:: ldux;
    "ldux64a"        >:: ldux_big_addr;

    "lhbrx64"        >:: lhbrx `ppc64;
    "lwbrx64"        >:: lwbrx `ppc64;
    "ldbrx64"        >:: ldbrx;
  ]
