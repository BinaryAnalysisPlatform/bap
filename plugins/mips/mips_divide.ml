open Mips.Std

(* DIV rs, rt
 * Divide Word, MIPS32, removed in Release 6
 * Page 178 *)
let div cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let rt = signed cpu.reg ops.(1) in
  let x = signed var doubleword in
  RTL.[
    x := zero;
    cpu.lo := rs /$ rt;
    cpu.hi := rs %$ rt;
  ]

(* DIV rd, rs, rt
 * Divide Words Signed, MIPS32, Release 6
 * Page 180 *)
let div_r6 cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[
    rd := rs /$ rt;
  ]

(* MOD rd, rs, rt
 * Modulo Words Signed, MIPS32, Release 6
 * Page 180 *)
let modulo cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[
    rd := rs %$ rt;
  ]

(* DIV rs, rt
 * Divide Word, MIPS32, removed in Release 6
 * Page 184 *)
let divu cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let rt = unsigned cpu.reg ops.(1) in
  let x = unsigned var doubleword in
  RTL.[
    x := zero;
    cpu.lo := rs / rt;
    cpu.hi := rs % rt;
  ]

(* DIVU rd, rs, rt
 * Divide Words Unsigned, MIPS32, Release 6
 * Page 180 *)
let divu_r6 cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs / rt;
  ]

(* MODU rd, rs, rt
 * Modulo Words Unsigned, MIPS32, Release 6
 * Page 180 *)
let modulou cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs % rt;
  ]

(* DDIV rd, rs, rt
 * Divide Doubleords Signed, MIPS64, Release 6
 * Page 180 *)
let ddiv cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[
    rd := rs /$ rt;
  ]

(* DMOD rd, rs, rt
 * Modulo Doublewords Signed, MIPS64, Release 6
 * Page 180 *)
let dmodulo cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = signed cpu.reg ops.(2) in
  RTL.[
    rd := rs %$ rt;
  ]

(* DDIVU rd, rs, rt
 * Divide Doublewords Unsigned, MIPS64, Release 6
 * Page 180 *)
let ddivu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs / rt;
  ]

(* DMODU rd, rs, rt
 * Modulo Doublewords Unsigned, MIPS64, Release 6
 * Page 180 *)
let dmodulou cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    rd := rs % rt;
  ]

let () =
  "SDIV" >> div;
  "MOD"  >> modulo;
  "UDIV" >> divu;
  "MODu" >> modulou;
  "DDIV" >> ddiv;
  "DMOD" >> dmodulo;
  "DDIVu" >> ddivu;
  "DMODu" >> dmodulou;
