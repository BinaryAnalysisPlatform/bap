open Core_kernel.Std
open Bap.Std
open Op

open Powerpc_types
open Hardware

let addr_of_exp addr_size exp = match addr_size with
  | `r32 -> Dsl.low32 exp
  | `r64 -> exp

(** Fixed-point Load Byte/Halfword/Word and Zero
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    89 3c 00 14 - lbz r9, 20(r28)
    89 20 00 14 - lbz r9, 20(0)
    a1 3c 00 14 - lhz r9, 20(r28)
    83 eb ff fc - lwz r31, -4(r11) *)
let lz addr_size endian size rt imm ra =
  let rt = Dsl.find rt in
  let ra = if Dsl.exists ra then Dsl.(var @@ find ra)
    else Dsl.int (Word.zero 64) in
  let bits = Size.in_bits addr_size in
  let imm = Word.of_int64 (Imm.to_int64 imm) in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + cast signed gpr_bitwidth (int imm));
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian size);
  ]

(** Fixed-point Load Byte/Halfword/Word and Zero Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3d 50 ae   lbzx r9, r29, r10
    7d 3d 52 2e   lhzx r9, r29, r10
    7d 3d 50 2e   lwzx r9, r29, r10  *)
let lzx addr_size endian size rt ra rb =
  let rt = Dsl.find rt in
  let ra = if Dsl.exists ra then Dsl.(var @@ find ra)
    else Dsl.int (Word.zero 64) in
  let rb = Dsl.find rb in
  let bits = Size.in_bits addr_size in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + var rb);
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian size);
  ]

(** Fixed-point Load Byte/Halfword/Word and Zero with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    8d 3c 00 14  lbzu r9, 20(r28)
    a5 3c 00 14  lhzu r9, 20(r28)
    85 3f ff fc  lwzu r9, -4(r31)  *)
let lzu addr_size endian size rt imm ra =
  if Reg.equal rt ra then Dsl.powerpc_fail "Invalid instruction lzu: same operands";
  let rt = Dsl.find rt in
  let ra = Dsl.find ra in
  let bits = Size.in_bits addr_size in
  let imm = Word.of_int64 (Imm.to_int64 imm) in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + cast signed gpr_bitwidth (int imm));
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian size);
    ra := cast unsigned gpr_bitwidth (var ea);
  ]

(** Fixed-point Load Byte/Halfword/Word and Zero with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3d 50 ee  lbzux r9, r29, r10
    7d 3d 52 6e  lhzux r9, r29, r10
    7d 3d 50 6e  lwzux r9, r29, r10  *)
let lzux addr_size endian size rt ra rb =
  if Reg.equal rt ra then Dsl.powerpc_fail "Invalid instruction lzux: same operands";
  let rt = Dsl.find rt in
  let ra = Dsl.find ra in
  let rb = Dsl.find rb in
  let bits = Size.in_bits addr_size in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + var rb);
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian size);
    ra := cast unsigned gpr_bitwidth (var ea);
  ]

(** Fixed-point Load Halfword Algebraic
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    a8 29 00 05    lha r1, 5(r9) *)
let lha addr_size endian rt ra imm =
  let rt = Dsl.find rt in
  let ra = if Dsl.exists ra then Dsl.(var @@ find ra)
    else Dsl.int (Word.zero 64) in
  let imm = Word.of_int64 (Imm.to_int64 imm) in
  let addr_bits = Size.in_bits addr_size in
  let ones = Word.ones 48 in
  let zeros = Word.zero 48 in
  let data = Dsl.fresh "data" (Type.imm 16) in
  let ea = Dsl.fresh "ea" (Type.imm addr_bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + cast signed gpr_bitwidth (int imm));
    data := load addr_size ~addr:(var ea) endian `r16;
    if_ (extract 15 15 (var data) = int Word.b0) [
      rt := int zeros ^ var data;
    ] [
      rt := int ones ^ var data;
    ]
  ]

(** Fixed-point Load Word Algebraic
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    eb eb 01 16    lwa r31, 276(r11) *)
let lwa addr_size endian rt ra imm =
  let rt = Dsl.find rt in
  let ra = if Dsl.exists ra then Dsl.(var @@ find ra)
    else Dsl.int (Word.zero 64) in
  let imm =
    let x = Imm.to_int64 imm in
    Word.of_int64 Int64.(x lsl 2)  in
  let addr_bits = Size.in_bits addr_size in
  let ones = Word.ones 32 in
  let zeros = Word.zero 32 in
  let data = Dsl.fresh "data" (Type.imm 32) in
  let ea = Dsl.fresh "ea" (Type.imm addr_bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + cast signed gpr_bitwidth (int imm));
    data := load addr_size ~addr:(var ea) endian `r32;
    if_ (extract 31 31 (var data) = int Word.b0) [
      rt := int zeros ^ var data;
    ] [
      rt := int ones ^ var data;
    ]
  ]

(** Fixed-point Load Halfword/Word Algebraic Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 25 4a ae    lhax r1, r5, r9
    7c 25 4a aa    lwax r1, r5, r9  *)
let lax addr_size endian size rt ra rb =
  let rt = Dsl.find rt in
  let ra = if Dsl.exists ra then Dsl.(var @@ find ra)
    else Dsl.int (Word.zero 64) in
  let rb = Dsl.find rb in
  let addr_bits = Size.in_bits addr_size in
  let data_bits = Size.in_bits size in
  let hbit = Size.in_bits size - 1 in
  let ones = Word.ones (gpr_bitwidth - data_bits) in
  let zeros = Word.zero (gpr_bitwidth - data_bits) in
  let ea = Dsl.fresh "ea" (Type.imm addr_bits) in
  let data = Dsl.fresh "data" (Type.imm data_bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + var rb);
    data := load addr_size ~addr:(var ea) endian size;
    if_ (extract hbit hbit (var data) = int Word.b0) [
      rt := int zeros ^ var data;
    ] [
      rt := int ones ^ var data;
    ]
  ]

(** Fixed-point Load Halfword Algebraic with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    ac 29 00 05    lhau r1, 5(r9) *)
let lhau addr_size endian rt ra imm =
  if Reg.equal rt ra then Dsl.powerpc_fail "Invalid instruction lhau: same operands";
  let size = `r16 in
  let rt = Dsl.find rt in
  let ra = Dsl.find ra in
  let imm = Word.of_int64 ~width:gpr_bitwidth (Imm.to_int64 imm) in
  let addr_bits = Size.in_bits addr_size in
  let data_bits = Size.in_bits size in
  let hbit = Size.in_bits size - 1 in
  let ones = Word.ones (gpr_bitwidth - data_bits) in
  let zeros = Word.zero (gpr_bitwidth - data_bits) in
  let ea = Dsl.fresh "ea" (Type.imm addr_bits) in
  let data = Dsl.fresh "data" (Type.imm data_bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + cast signed gpr_bitwidth (int imm));
    data := load addr_size ~addr:(var ea) endian size;
    if_ (extract hbit hbit (var data) = int Word.b0) [
      rt := int zeros ^ var data;
    ] [
      rt := int ones ^ var data;
    ];
    ra := cast unsigned gpr_bitwidth (var ea)
  ]

(** Fixed-point Load Data/Word Algebraic with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 25 4a ee    lhaux r1, r5, r9
    7c 25 4a ea    lwaux r1, r5, r9 *)
let laux addr_size endian size rt ra rb =
  if Reg.equal rt ra then Dsl.powerpc_fail "Invalid instruction lhaux: same operands";
  let rt = Dsl.find rt in
  let ra = Dsl.find ra in
  let rb = Dsl.find rb in
  let addr_bits = Size.in_bits addr_size in
  let data_bits = Size.in_bits size in
  let hbit = Size.in_bits size - 1 in
  let ones = Word.ones (gpr_bitwidth - data_bits) in
  let zeros = Word.zero (gpr_bitwidth - data_bits) in
  let ea = Dsl.fresh "ea" (Type.imm addr_bits) in
  let data = Dsl.fresh "data" (Type.imm data_bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + var rb);
    data := load addr_size ~addr:(var ea) endian size;
    if_ (extract hbit hbit (var data) = int Word.b0) [
      rt := int zeros ^ var data;
    ] [
      rt := int ones ^ var data;
    ];
    ra := cast unsigned gpr_bitwidth (var ea)
  ]

(** Fixed-point Load Dobuleword
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    e8 29 00 08    ld r1, 8(r9) *)
let ld addr_size endian rt ra imm =
  let rt = Dsl.find rt in
  let ra = if Dsl.exists ra then Dsl.(var @@ find ra)
    else Dsl.int (Word.zero 64) in
  let bits = Size.in_bits addr_size in
  let imm =
    let x = Imm.to_int64 imm in
    Word.of_int64 Int64.(x lsl 2)  in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + cast signed gpr_bitwidth (int imm));
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64);
  ]

(** Fixed-point Load Dobuleword Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 28 48 2a    ldx r1, r8, r9 *)
let ldx addr_size endian rt ra rb =
  let rt = Dsl.find rt in
  let ra = if Dsl.exists ra then Dsl.(var @@ find ra)
    else Dsl.int (Word.zero 64) in
  let rb = Dsl.find rb in
  let bits = Size.in_bits addr_size in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (ra + var rb);
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64);
  ]

(** Fixed-point Load Dobuleword with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    e8 29 00 09    ldu r1, 8(r9) *)
let ldu addr_size endian rt ra imm =
  if Reg.equal rt ra then Dsl.powerpc_fail "Invalid instruction ldu: same operands";
  let rt = Dsl.find rt in
  let ra = Dsl.find ra in
  let bits = Size.in_bits addr_size in
  let imm =
    let x = Imm.to_int64 imm in
    Word.of_int64 Int64.(x lsl 2)  in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + cast signed gpr_bitwidth (int imm));
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64);
    ra := cast unsigned gpr_bitwidth (var ea);
  ]

(** Fixed-point Load Dobuleword with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 28 48 6a    ldux r1, r8, r9 *)
let ldux addr_size endian rt ra rb =
  if Reg.equal rt ra then Dsl.powerpc_fail "Invalid instruction ldux: same operands";
  let rt = Dsl.find rt in
  let ra = Dsl.find ra in
  let rb = Dsl.find rb in
  let bits = Size.in_bits addr_size in
  let ea = Dsl.fresh "ea" (Type.imm bits) in
  Dsl.[
    ea := addr_of_exp addr_size (var ra + var rb);
    rt := cast unsigned gpr_bitwidth (load addr_size ~addr:(var ea) endian `r64);
    ra := cast unsigned gpr_bitwidth (var ea);
  ]

type lz = [
  | `LBZ
  | `LHZ
  | `LWZ
] [@@deriving sexp, enumerate ]

type lzx = [
  | `LBZX
  | `LHZX
  | `LWZX
] [@@deriving sexp, enumerate ]

type lzu = [
  | `LBZU
  | `LHZU
  | `LWZU
] [@@deriving sexp, enumerate ]

type lzux = [
  | `LBZUX
  | `LHZUX
  | `LWZUX
] [@@deriving sexp, enumerate]

type la = [
  | `LHA
  | `LWA
] [@@deriving sexp, enumerate]

type lax = [
  | `LHAX
  | `LWAX
] [@@deriving sexp, enumerate]

type lhau = [
  | `LHAU
] [@@deriving sexp, enumerate]

type laux = [
  | `LHAUX
  | `LWAUX
] [@@deriving sexp, enumerate]

type ld = [
  | `LD
  | `LDX
  | `LDU
  | `LDUX
] [@@deriving sexp, enumerate]

type t = [ lz | lzx | lzu | lzux | la | lax | lhau | laux | ld ] [@@deriving sexp, enumerate]

let size_of_t = function
  | `LBZ | `LBZX | `LBZU | `LBZUX -> `r8
  | `LHZ | `LHZX | `LHZU | `LHZUX
  | `LHA | `LHAX | `LHAU | `LHAUX -> `r16
  | `LWZ | `LWZX | `LWZU | `LWZUX
  | `LWA | `LWAX | `LWAUX         -> `r32
  | `LD  | `LDX  | `LDU  | `LDUX  -> `r64

let lift opcode mode endian mem ops =
  let size = size_of_t opcode in
  match opcode, ops with
  | #lz,   [| Reg rt; Imm imm; Reg ra; |] -> lz mode endian size rt imm ra
  | #lzx,  [| Reg rt; Reg ra; Reg rb; |] -> lzx mode endian size rt ra rb
  | #lzu,  [| Reg rt; Reg _; Imm imm; Reg ra; |] -> lzu mode endian size rt imm ra
  | #lzux, [| Reg rt; Reg _; Reg ra; Reg rb |] -> lzux mode endian size rt ra rb
  | `LHA,  [| Reg rt; Imm imm; Reg ra; |] -> lha mode endian rt ra imm
  | `LWA,  [| Reg rt; Imm imm; Reg ra; |] -> lwa mode endian rt ra imm
  | #lax,  [| Reg rt; Reg ra; Reg rb;  |] -> lax mode endian size rt ra rb
  | `LHAU, [| Reg rt; Reg _; Imm imm; Reg ra; |] -> lhau mode endian rt ra imm
  | #laux, [| Reg rt; Reg _; Reg ra; Reg rb;  |] -> laux mode endian size rt ra rb
  | `LD,   [| Reg rt; Imm imm; Reg ra; |] -> ld mode endian rt ra imm
  | `LDX,  [| Reg rt; Reg ra; Reg rb; |] -> ldx mode endian rt ra rb
  | `LDU,  [| Reg rt; Reg _; Imm imm; Reg ra; |] -> ldu mode endian rt ra imm
  | `LDUX, [| Reg rt; Reg _; Reg ra; Reg rb; |] -> ldux mode endian rt ra rb
  | opcode, _ ->
    let opcode = Sexp.to_string (sexp_of_t opcode) in
    Dsl.powerpc_fail "%s: unexpected operand set" opcode
