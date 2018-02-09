
open Powerpc.Std

(** Fixed-Point Arithmetic Instructions - Substract From
    Page 69 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 50    subf  r1, r2, r3
    7c 22 18 51    subf. r1, r2, r3 *)
let subf cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[rt := lnot ra + rb + one]

(** Fixed-Point Arithmetic Instructions - Substract From Immediate Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    example:
    20 22 10 92    subfic r1, r2, 4242 *)
let subfic cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let si = signed imm ops.(2) in
  RTL.[
    rt := lnot ra + si + one;
    cpu.ca   := low cpu.word_width si < ra;
    cpu.ca32 := low word si < low word ra;
  ]

(** Fixed-Point Arithmetic Instructions - Substract From Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 10    subfc  r1, r2, r3
    7c 22 18 11    subfc. r1, r2, r3 *)
let subfc cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    rt := lnot ra + rb + one;
    cpu.ca   := low cpu.word_width rb < low cpu.word_width ra;
    cpu.ca32 := low word rb < low word ra;
  ]

(** Fixed-Point Arithmetic Instructions - Substract From Extended
    Page 71 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 10    subfe  r1, r2, r3
    7c 22 19 11    subfe. r1, r2, r3 *)
let subfe cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    rt := lnot ra + rb + cpu.ca;
    cpu.ca32 := low word rb < low word (ra + one - cpu.ca);
    cpu.ca   := rb < ra + one - cpu.ca;
  ]

(** Fixed-Point Arithmetic Instructions - Substract From Minus One Extended
    Page 71 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 d0    subfme  r1, r2
    7c 22 01 d1    subfme. r1, r2 *)
let subfme cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  RTL.[
    rt := lnot ra + cpu.ca - one;
    cpu.ca32 := one;
    cpu.ca   := one;
  ]

(** Fixed-Point Arithmetic Instructions - Substract From Zero Extended
    Page 71 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 90    subfze  r1, r2
    7c 22 01 91    subfze. r1, r2 *)
let subfze cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  RTL.[
    rt := lnot ra + cpu.ca;
    cpu.ca32 := one;
    cpu.ca   := one;
  ]

let init () =
  "SUBF"    >| subf;
  "SUBFo"   >. subf;
  "SUBFIC"  >| subfic;
  "SUBFC"   >| subfc;
  "SUBFCo"  >. subfc;
  "SUBFE"   >| subfe;
  "SUBFEo"  >. subfe;
  "SUBFME"  >| subfme;
  "SUBFMEo" >. subfme;
  "SUBFZE"  >| subfze;
  "SUBFZEo" >. subfze;
