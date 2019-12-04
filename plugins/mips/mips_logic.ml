open Mips.Std

(* AND rd, rs, rt
 * And, MIPS32
 * Page 47 *)
let mand cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs land rt;
  ]

(* ANDI rd, rs, imm
 * And Immediate, MIPS32
 * Page 48 *)
let mandi cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    rd := rs land im;
  ]

(* TODO: EXT rd, rs, p, s *)
(* TODO: INS rd, rs, p, s *)

(* NOP
 * Just NOP, here and everywhere
 * Page 364 *)
let nop cpu ops = []

(* NOR rd, rs, rt
 * Not Or, MIPS32
 * Page 365 *)
let nor cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := lnot (rs lor rt);
  ]

(* NOT rd, rs
 * Not in the manual *)
let mnot cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  RTL.[
    rd := lnot rs;
  ]

(* OR rd, rs, rt
 * Or, MIPS32
 * Page 366 *)
let mor cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs lor rt;
  ]

(* ORI rd, rs, imm
 * Or Immediate, MIPS32
 * Page 367 *)
let mori cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    rd := rs lor im;
  ]

(* WSBH rd, rs
 * Word Swap Bytes Within Halfwords, MIPS32 Release 2
 * Page 509 *)
let wsbh cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  RTL.[
    rd := nth byte rs 2 ^ nth byte rs 3 ^
      nth byte rs 0 ^ nth byte rs 1;
  ]

(* DSBH rd, rs
 * Doubleword Swap Bytes Within Halfwords, MIPS64 Release 2
 * Page 196 *)
let dsbh cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  RTL.[
    rd := nth byte rs 6 ^ nth byte rs 7 ^
      nth byte rs 4 ^ nth byte rs 5 ^
      nth byte rs 2 ^ nth byte rs 3 ^
      nth byte rs 0 ^ nth byte rs 1;
  ]

(* DSHD rd, rs
 * Doubleword Swap Halfword Within Doublewords, MIPS64 Release 2
 * Page 509 *)
let dshd cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  RTL.[
    rd := nth halfword rs 0 ^ nth halfword rs 1 ^
      nth halfword rs 2 ^ nth halfword rs 3;
  ]

(* XOR rd, rs, rt
 * Exclusive Or, MIPS32
 * Page 509 *)
let mxor cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs lxor rt;
  ]

(* XORI rd, rs, imm
 * Exclusive Or Immediate, MIPS32
 * Page 511 *)
let mxori cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    rd := rs lxor im;
  ]

(* BITSWAP rd, rt
 * Swaps (reverses) bits in each byte, MIPS32 Release 6
 * Page 95 *)
let bitswap cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rt = unsigned cpu.reg ops.(1) in
  let cnt = unsigned var byte in
  let biti = unsigned var bit in
  let width = unsigned const byte 8 in
  let pos = unsigned var byte in
  let max_ind = unsigned const byte 7 in
  let byte_ind = unsigned var byte in
  RTL.[
    cnt := zero;
    foreach biti rd [
      byte_ind := cnt / width;
      pos := byte_ind * width + (max_ind - (cnt % width));
      biti := lsb (rt lsr pos);
      cnt := cnt + one;
    ];
  ]

(* DBITSWAP rd, rt  - 64bit instruction
 * Swaps (reverses) bits in each byte, MIPS64 Release 6
 * Page 95 *)
let dbitswap cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rt = unsigned cpu.reg ops.(1) in
  let cnt = unsigned var byte in
  let biti = unsigned var bit in
  let width = unsigned const byte 8 in
  let pos = unsigned var byte in
  let max_ind = unsigned const byte 7 in
  let byte_ind = unsigned var byte in
  RTL.[
    cnt := zero;
    foreach biti rd [
      byte_ind := cnt / width;
      pos := byte_ind * width + (max_ind - (cnt % width));
      biti := lsb (rt lsr pos);
      cnt := cnt + one;
    ];
  ]

let () =
  Bap_main.Extension.declare @@ fun _ctxt ->
  "AND" >> mand;
  "ANDi" >> mandi;
  "NOP" >> nop;
  "NOR" >> nor;
  "NOT" >> mnot;
  "OR" >> mor;
  "ORi" >> mori;
  "XOR" >> mxor;
  "XORi" >> mxori;
  "BITSWAP" >> bitswap;
  "DBITSWAP" >> dbitswap;
  "WSBH" >> wsbh;
  "DSBH" >> dsbh;
  Ok ()
