open Mips.Std

(* ADD rd, rs, rt
 * Add Word, MIPS32
 * Page 36 *)
let add cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[ rd := rs + rt; ]

(* ADDI rt, rs, immediate
 * Add Immediate Word, MIPS32, removed in Release 6
 * Page 38 *)
let addi cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[ rt := rs + im; ]

(* ADDIU rt, rs, immediate
 * Add Immediate Unsigned Word, MIPS32
 * Page 39 *)
let addiu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[ rt := rs + im; ]

(* ADDIUPC rs, immediate
 * Add Immediate to PC (unsigned), MIPS32 Release 6
 * Page 40 *)
let addiupc cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  RTL.[ rs := cpu.cia + im; ]

(* ADDU rd, rs, rt
 * Add Unsigned Word, MIPS32,
 * Page 41 *)
let addu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[ rd := rs + rt; ]

(* CLO rd, rs
 * Count Leading Ones in Word, MIPS32
 * Page 136 *)
(* MIPS produces Undefined Behavior if RT <> RD *)
let clo cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let _rt = unsigned cpu.reg ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  let xv = unsigned var word in
  let cnt = unsigned var byte in
  let has_no_zeroes = unsigned var bit in
  let biti = unsigned var bit in
  RTL.[
    xv := low word rs;
    cnt := zero;
    has_no_zeroes := one;
    foreach biti xv [
      if_ (has_no_zeroes land (biti = one)) [
        cnt := cnt + one;
      ] [
        has_no_zeroes := zero;
      ]
    ];
    rd := cnt;
  ]

(* CLZ rd, rs
 * Count Leading Zeroes in Word, MIPS32
 * Page 137 *)
(* MIPS produces Undefined Behavior if RT <> RD *)
let clz cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let _rt = unsigned cpu.reg ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  let xv = unsigned var word in
  let cnt = unsigned var byte in
  let has_no_ones = unsigned var bit in
  let biti = unsigned var bit in
  RTL.[
    xv := low word rs;
    cnt := zero;
    has_no_ones := one;
    foreach biti xv [
      if_ (has_no_ones land (biti = zero)) [
        cnt := cnt + one;
      ] [
        has_no_ones := zero;
      ]
    ];
    rd := cnt;
  ]

(* LUi rt, imm
 * Load Upper Immediate, MIPS32
 * Page 279 *)
let lui cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  RTL.[
    rt := im lsl unsigned const byte 16;
  ]

(* ALUIPC rs, imm
 * Aligned Add Upper Immediate to PC, MIPS32 Release 6
 * Page 45 *)
let aluipc cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let x = unsigned var word in
  let mask = unsigned var word in
  RTL.[
    mask := unsigned const word 0xFFFF;
    x := cpu.cia + (im lsl unsigned const byte 16);
    rs := (lnot mask) land x;
  ]

(* LSA rd, rs, rt, sa
 * Load Scaled Address, MIPS32 Release 6
 * Page 278 *)
let lsa cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let sa = unsigned imm ops.(3) in
  RTL.[
    rd := (rs lsl (sa + unsigned const byte 1)) + rt;
  ]

(* DLSA rd, rs, rt, sa
 * Load Scaled Address, MIPS64 Release 6
 * Page 278 *)
let dlsa cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let sa = unsigned imm ops.(3) in
  RTL.[
    rd := (rs lsl (sa + unsigned const byte 1)) + rt;
  ]

(* MOVE rd, rs  - WTF nonexistent!? *)

(* SEB rd, rt
 * Sign-Extend Byte, MIPS32 Release 2
 * Page 426 *)
let seb cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  RTL.[
    rs := last rt 8;
  ]

(* SEH rd, rt
 * Sign-Extend Halfword, MIPS32 Release 2
 * Page 427 *)
let seh cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  RTL.[
    rs := last rt 16;
  ]

(* SUB rd, rs, rt
 * Subtract Word, MIPS32
 * Page 452 *)
let sub cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[ rd := rs - rt; ]

(* SUBU rd, rs, rt
 * Subtract Word Unsigned, MIPS32
 * Page 454 *)
let subu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[ rd := rs - rt; ]

(* DSUB rd, rs, rt
 * Subtract Doubleword, MIPS64
 * Page 207 *)
let dsub cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[ rd := rs - rt; ]

(* DSUBU rd, rs, rt
 * Subtract Doubleword Unsigned, MIPS64
 * Page 208 *)
let dsubu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[ rd := rs - rt; ]

let () =
  "ADD" >> add;
  "ADDi" >> addi;
  "ADDiu" >> addiu;
  "ADDiupc" >> addiupc;
  "ADDu" >> addu;
  "CLO" >> clo;
  "CLZ" >> clz;
  "LSA" >> lsa;
  "DLSA" >> dlsa;
  "LUi" >> lui;
  "ALUipc" >> aluipc;
  "SEB" >> seb;
  "SEH" >> seh;
  "SUB" >> sub;
  "SUBu" >> subu;
  "DSUB" >> dsub;
  "DSUBu" >> dsubu;
