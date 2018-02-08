open Powerpc.Std

(** Conditional Cpu.Register Instructions - And
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 1a 02  crand 1,2,3 *)
let crand cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := ba land bb;
  ]

(** Conditional Cpu.Register Instructions - NAnd
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 19 c2  crnand 1,2,3 *)
let crnand cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := lnot (ba land bb);
  ]

(** Conditional Cpu.Register Instructions - Or
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 1b 82  cror 1,2,3 *)
let cror cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := ba lor bb;
  ]

(** Conditional Cpu.Register Instructions - Xor
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 19 82   crxor 1,2,3 *)
let crxor cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := ba lxor bb;
  ]

(** Conditional Cpu.Register Instructions - NOr
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 18 42    crnor 1,2,3  *)
let crnor cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := lnot (ba lor bb);
  ]

(** Conditional Cpu.Register Instructions - Eqv
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 1a 42    creqv 1,2,3 *)
let creqv cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := ba lxor (lnot bb);
  ]

(** Conditional Cpu.Register Instructions - And with Complement
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 19 02   crandc 1,2,3 *)
let crandc cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := ba land (lnot bb);
  ]

(** Conditional Cpu.Register Instructions - Or with Complement
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 22 1b 42   crorc 1,2,3 *)
let crorc cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let ba = unsigned cpu.reg ops.(1) in
  let bb = unsigned cpu.reg ops.(2) in
  RTL.[
    bt := ba lor (lnot bb);
  ]

(** Conditional Cpu.Register Instructions - Move CR Field
    Pages 40-41 of IBM Power ISATM Version 3.0 B
    example:
    4c 88 00 00   mcrf 1,2 *)
let mcrf cpu ops =
  let bt = unsigned cpu.reg ops.(0) in
  let bs = unsigned cpu.reg ops.(1) in
  RTL.[
    bt := bs;
  ]

let () =
  "CRAND"  >| crand;
  "CRNAND" >| crnand;
  "CROR"   >| cror;
  "CRXOR"  >| crxor;
  "CRNOR"  >| crnor;
  "CREQV"  >| creqv;
  "CRANDC" >| crandc;
  "CRORC"  >| crorc;
  "MCRF"   >| mcrf;
