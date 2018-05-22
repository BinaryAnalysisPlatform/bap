open Powerpc.Std

(** Fix-point Compare Immediate
    Page 85 of IBM Power ISATM Version 3.0 B
    examples:
    2f 89 ff ff     cmpwi cr7, r9, -1
    2f a9 ff ff     cmpdi cr7, r9, -1 *)
let cmpwi cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let si = signed imm ops.(2) in
  RTL.[
    nth bit bf 0 := low word ra < si;
    nth bit bf 1 := low word ra > si;
    nth bit bf 2 := low word ra = si;
    nth bit bf 3 := cpu.so;
  ]

let cmpdi cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let si = signed imm ops.(2) in
  RTL.[
    nth bit bf 0 := ra < si;
    nth bit bf 1 := ra > si;
    nth bit bf 2 := ra = si;
    nth bit bf 3 := cpu.so;
  ]

(** Fix-point Compare
    Page 85 of IBM Power ISATM Version 3.0 B
    examples:
    7f 86 38 00     cmpw cr7, r6, r7
    7f a6 38 00     cmpd cr7, r6, r7 *)
let cmpw cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    nth bit bf 0 := low word ra < rb;
    nth bit bf 1 := low word ra > rb;
    nth bit bf 2 := low word ra = rb;
    nth bit bf 3 := cpu.so;
  ]

let cmpd cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    nth bit bf 0 := ra < rb;
    nth bit bf 1 := ra > rb;
    nth bit bf 2 := ra = rb;
    nth bit bf 3 := cpu.so;
  ]

(** Fix-point Compare Logical Immediate
    Page 86 of IBM Power ISATM Version 3.0 B
    examples:
    2b 89 00 01     cmplwi cr7, r9, 1
    2b a9 00 01     cmpldi cr7, r9, 1 *)
let cmplwi cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let ui = unsigned imm ops.(2) in
  RTL.[
    nth bit bf 0 := low word ra < ui;
    nth bit bf 1 := low word ra > ui;
    nth bit bf 2 := low word ra = ui;
    nth bit bf 3 := cpu.so;
  ]

let cmpldi cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let ui = unsigned imm ops.(2) in
  RTL.[
    nth bit bf 0 := ra < ui;
    nth bit bf 1 := ra > ui;
    nth bit bf 2 := ra = ui;
    nth bit bf 3 := cpu.so;
  ]

(** Fix-point Compare Logical
    Page 86 of IBM Power ISATM Version 3.0 B
    examples:
    7f 86 38 40     cmplw cr7, r6, r7
    7f a6 38 40     cmpld cr7, r6, r7 *)
let cmplw cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    nth bit bf 0 := low word ra < rb;
    nth bit bf 1 := low word ra > rb;
    nth bit bf 2 := low word ra = rb;
    nth bit bf 3 := cpu.so;
  ]

let cmpld cpu ops =
  let bf = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    nth bit bf 0 := ra < rb;
    nth bit bf 1 := ra > rb;
    nth bit bf 2 := ra = rb;
    nth bit bf 3 := cpu.so;
  ]

let init () =
  "CMPWI"  >| cmpwi;
  "CMPDI"  >| cmpdi;
  "CMPW"   >| cmpw;
  "CMPD"   >| cmpd;
  "CMPLWI" >| cmplwi;
  "CMPLDI" >| cmpldi;
  "CMPLW"  >| cmplw;
  "CMPLD"  >| cmpld;
