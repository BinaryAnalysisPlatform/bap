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

let () =
  "ROTR" >> rotr;
  "ROTRV" >> rotrv;
  "SLL" >> sll;
  "SLLV" >> sllv;
  "SRA" >> sra;
  "SRAV" >> srav;
  "SRL" >> srl;
  "SRLV" >> srlv;
