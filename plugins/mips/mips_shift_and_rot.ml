open Mips.Std

(* ROTR rd, rt, imm *)
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
    (* sign extend *)
    rd := tm lor tt;
  ]

(* ROTRV rd, rs, rt *)
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
    (* sign extend *)
    rd := tm lor tt;
  ]

(* SLL rd, rs, imm *)
let sll cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rs lsl sa;
    (* sign extend *)
    rd := tm;
  ]

(* SLLV rd, rs, rt *)
let sllv cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rt lsl rs;
    (* sign extend *)
    rd := tm;
  ]

(* SRA rd, rs, imm *)
let sra cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rs lsr sa;
    (* sign extend *)
    rd := tm;
  ]

(* SRAV rd, rs, rt *)
let srav cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rt lsr rs;
    (* sign extend *)
    rd := tm;
  ]

(* SRL rd, rs, imm *)
let srl cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sa = unsigned imm ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rs lsr sa;
    (* sign extend *)
    rd := tm;
  ]

(* SRLV rd, rs, rt *)
let srlv cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  let tm = unsigned var word in
  RTL.[
    tm := rt lsr rs;
    (* sign extend *)
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
