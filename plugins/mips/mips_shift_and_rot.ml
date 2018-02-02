open Mips.Std

(* ROTR rd, rt, imm
 * Rotate Word Right, MIPS32 Release 2
 * Page 391 *)
let rotr cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rt = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  let tm = unsigned var word in
  let tt = unsigned var word in
  RTL.[
    (* extract low 32bit word *)
    tm := last rt 32;
    (* rotate right *)
    tt := tm lsr sa;
    tm := tm lsl (unsigned const byte 32 - sa);
    rd := tm lor tt;
  ]

(* ROTRV rd, rs, rt
 * Rotate Word Right Variable, MIPS32 Release 2
 * Page 392 *)
let rotrv cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  let tt = unsigned var word in
  RTL.[
    (* extract low 32bit word *)
    tm := last rt 32;
    (* rotate right at bits in rs *)
    tt := tm lsr rs;
    tm := tm lsl (unsigned const byte 32 - rs);
    rd := tm lor tt;
  ]

(* SLL rd, rs, imm
 * Shift Word Left Logical, MIPS32
 * Page 440 *)
let sll cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rs lsl sa;
    (* TODO: sign extend *)
    rd := tm;
  ]

(* SLLV rd, rs, rt
 * Shift Word Left Logical Variable, MIPS32
 * Page 441 *)
let sllv cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rt lsl rs;
    (* TODO: sign extend *)
    rd := tm;
  ]

(* DSLL rd, rs, imm
 * Shift Doubleword Left Logical, MIPS64
 * Page 198 *)
let dsll cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  RTL.[
    rd := rs lsl sa;
  ]

(* DSLL32 rd, rs, imm
 * Shift Doubleword Left Logical Plus 32, MIPS64
 * Page 199 *)
let dsll32 cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  RTL.[
    rd := rs lsl (sa + unsigned const byte 32);
  ]

(* DSLLV rd, rs, rt
 * Shift Doubleword Left Logical Variable, MIPS64
 * Page 200 *)
let dsllv cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rt lsl rs;
  ]

(* SRA rd, rs, imm
 * Shift Word Right Arithmetic, MIPS32
 * Page 446 *)
let sra cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rs lsr sa;
    (* TODO: sign extend *)
    rd := tm;
  ]

(* SRAV rd, rs, rt
 * Shift Word Right Arithmetic Variable, MIPS32
 * Page 448 *)
let srav cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rt lsr rs;
    (* TODO: sign extend *)
    rd := tm;
  ]

(* DSRA rd, rs, imm
 * Shift Doubleword Right Arithmetic, MIPS64
 * Page 201 *)
let dsra cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  RTL.[
    rd := rs lsr sa;
  ]

(* DSRA32 rd, rs, imm
 * Shift Doubleword Right Arithmetic Plus 32, MIPS64
 * Page 202 *)
let dsra32 cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  RTL.[
    rd := rs lsr (sa + unsigned const byte 32);
  ]

(* DSRAV rd, rs, rt
 * Shift Doubleword Right Arithmetic Variable, MIPS64
 * Page 203 *)
let dsrav cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rt lsr rs;
  ]

(* SRL rd, rs, imm
 * Shift Word Right Logical, MIPS32
 * Page 449 *)
let srl cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rs lsr sa;
    (* TODO: sign extend *)
    rd := tm;
  ]

(* SRLV rd, rs, rt
 * Shift Word Right Logical Variable, MIPS32
 * Page 450 *)
let srlv cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rt lsr rs;
    (* TODO: sign extend *)
    rd := tm;
  ]

(* DSRL rd, rs, imm
 * Shift Doubleword Right Logical, MIPS64
 * Page 203 *)
let dsrl cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  RTL.[
    rd := rs lsr sa;
  ]

(* DSRL32 rd, rs, imm
 * Shift Doubleword Right Logical, MIPS64
 * Page 204 *)
let dsrl32 cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  RTL.[
    rd := rs lsr (sa + unsigned const byte 32);
  ]

(* DSRLV rd, rs, rt
 * Shift Doubleword Right Logical Variable, MIPS64
 * Page 205 *)
let dsrlv cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  RTL.[
    rd := rt lsr rs;
  ]

let () =
  "ROTR" >> rotr;
  "ROTRV" >> rotrv;
  "SLL" >> sll;
  "SLLV" >> sllv;
  "DSLL" >> dsll;
  "DSLL32" >> dsll32;
  "DSLLV" >> dsllv;
  "SRA" >> sra;
  "SRAV" >> srav;
  "DSRA" >> dsra;
  "DSRA32" >> dsra32;
  "DSRAV" >> dsrav;
  "SRL" >> srl;
  "SRLV" >> srlv;
  "DSRL" >> dsrl;
  "DSRL32" >> dsrl32;
  "DSRLV" >> dsrlv;
