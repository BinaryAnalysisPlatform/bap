open Mips.Std

(* ADD rd, rs, rt *)
let add cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[ rd := rs + rt; ]

(* ADD.S fd, fs, ft
 * ADD.D fd, fs, ft *)
let add_fmt cpu ops =
  RTL.[]

(* ADDI rt, rs, immediate *)
let addi cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[ rt := rs + im; ]

(* ADDIU rt, rs, immediate *)
let addiu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[ rt := rs + im; ]

(* ADDIUPC rs, immediate *)
let addiupc cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  RTL.[ rs := cpu.cia + im; ]

(* ADDU rd, rs, rt *)
let addu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[ rd := rs + rt; ]

(* CLO rd, rs *)
(* TODO: check that RT = RD *)
let clo cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let rt = unsigned cpu.reg ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  let tm = unsigned const halfword 32 in
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

(* CLZ rd, rs *)
(* TODO: check that RT = RD *)
let clz cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let rt = unsigned cpu.reg ops.(1) in
  let rd = unsigned cpu.reg ops.(2) in
  let tm = unsigned const halfword 32 in
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

(*
let la cpu ops =
let li cpu ops =
*)
(* LUi rt, imm *)
let lui cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  RTL.[
    rt := im lsl unsigned const byte 16;
  ]

(* MOVE rd, rs  - WTF nonexistent!? *)
(*
let move cpu ops =
let negu cpu ops =
*)
(* SEB rd, rt *)
let seb cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  RTL.[
    (* TODO: Add sign-extend *)
    rs := rt;
  ]

(* SEH rd, rt *)
let seh cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  RTL.[
    (* TODO: Add sign-extend *)
    rs := rt;
  ]

(* SUB rd, rs, rt *)
let sub cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[ rd := rs - rt; ]

(* SUBU rd, rs, rt *)
let subu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[ rd := rs - rt; ]

let () =
  "ADD" >> add;
  "ADD.S" >> add_fmt;
  "ADD.D" >> add_fmt;
  "ADDi" >> addi;
  "ADDiu" >> addiu;
  "ADDiupc" >> addiupc;
  "ADDu" >> addu;
  "CLO" >> clo;
  "CLZ" >> clz;
  "LUi" >> lui;
  "SEB" >> seb;
  "SEH" >> seh;
  "SUB" >> sub;
  "SUBu" >> subu;
