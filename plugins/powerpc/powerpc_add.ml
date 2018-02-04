(** There is some sort of confusion with add instructions. Some of them
    are not recognizable by llvm, other has name that similar to other
    instructions in ISA specification, so one can found useful next table
    of examples (oe and rc are 21 and 31 bits):

    --------------------------------------------------------------------------------
    |  opcode     |      asm          |  bits   |recognized by llvm| name in llvm  |
    |-------------|-------------------|---------|------------------|---------------|
    | 7d 62 5a 14 |add    r11, r2, r11|rc=0,oe=0|  add  11,2,11    | ADD4          |
    | 7d 62 5a 15 |add.   r11, r2, r11|rc=1,oe=0|  add. 11,2,11    | ADD4o         |
    | 7d 62 5e 14 |addo   r11, r2, r11|rc=0,oe=1|  not recognized    |  ---          |
    | 7d 62 5e 15 |addo.  r11, r2, r11|rc=1,oe=1|  not recognized    |  ---          |
    | 7d 62 58 14 |addc   r11, r2, r11|rc=0,oe=0| addc  11,2,11    | ADDC          |
    | 7d 62 58 15 |addc.  r11, r2, r11|rc=1,oe=0| addc.  11,2,11   | ADDCo         |
    | 7d 62 5c 14 |addco  r11, r2, r11|rc=0,oe=1|  not recognized    |  ---          |
    | 7d 62 5c 15 |addco. r11, r2, r11|rc=1,oe=1|  not recognized    |  ---          |
    --------------------------------------------------------------------------------

    And so on. Basicly, instructions
    addo addco addeo addmeo addzeo addo. addco. addeo. addmeo. addzeo.
    doesn't recognized by llvm.

    And theirs oe-less analogs - addc. adde. addme. addze. -
    contains "o" suffix in thers llvm names: ADDo ADDeo ADDMEo ADDZEo,
    where "o" stands for .(dot) . *)

open Powerpc.Std

(** Extended mnemonics:

    lis rx, value      = addis  rx, 0,  value
    li  rx, value      = addi   rx, 0,  value
    la  rx, disp(ry)   = addi   rx, ry, disp
    subis rx,ry,value  = addis  rx, ry, -value
    subic rx,ry,value  = addic  rx, ry, -value
    subic. rx,ry,value = addic. rx, ry, -value *)

(** Fixed-Point Arithmetic Instructions - Add Immediate
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    38 21 00 10     addi    r1,r1,16
    3b de fd 28     addi    r30,r30,-728
    38 20 00 10     addi    r1,0,16 (OR li r1, 16) *)
let addi cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[ rt := ra + im; ]

let li cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  RTL.[ rt := im; ]

(** Fixed-Point Arithmetic Instructions - Add Immediate Shifted
    Page 67 of IBM Power ISATM Version 3.0 B
    examples:
    3f de 00 02     addis   r30,r30,2
    3d 6b f0 00     addis   r11,r11,-4096 *)
let addis cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let sh = unsigned const word 16 in
  RTL.[ rt := ra + (im lsl sh); ]

let lis cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let sh = unsigned const word 16 in
  RTL.[ rt := im lsl sh; ]

(** Fixed-Point Arithmetic Instructions - Add
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    7d 62 5a 14 add   r11, r2, r11
    7d 62 5a 15 add.  r11, r2, r11 *)
let add cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[rt := ra + rb]

(** Fixed-Point Arithmetic Instructions - Add Immediate Carrying
    Page 69 of IBM Power ISATM Version 3.0 B
    examples:
    30 21 00 10    addic r1, r1, 16
    33 de fd 28    addic r30, r30, -728

    Add Immediate Carrying and Record:
    34 21 00 10    addic. r1, r1, 16
    37 de fd 28    addic. r30, r30, -728 *)
let addic cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let tm = signed var doubleword in
  RTL.[
    tm := ra;
    rt := ra + im;
    cpu.ca := low cpu.word_width rt < low cpu.word_width tm;
    cpu.ca32 := low word rt < low word tm;
  ]

(** Fixed-Point Arithmetic Instructions - Add Carrying
    Page 70 of IBM Power ISATM Version 3.0 B
    examples:
    7d 62 58 14  addc   r11, r2, r11
    7d 62 58 15  addc.  r11, r2, r11 *)
let addc cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let tm = signed var doubleword in
  RTL.[
    tm := ra;
    rt := ra + rb;
    cpu.ca := low cpu.word_width rt < low cpu.word_width tm;
    cpu.ca32 := low word rt < low word tm;
  ]

(** Fixed-Point Arithmetic Instructions - Add Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 21 81 14  adde   r11, r2, r11
    7c 21 81 15  adde.  r11, r2, r11 *)
let adde cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let tm = signed var doubleword in
  RTL.[
    tm := ra;
    rt := ra + rb + cpu.ca;
    cpu.ca := low cpu.word_width rt < low cpu.word_width tm;
    cpu.ca32 := low word rt < low word tm;
  ]

(** Fixed-Point Arithmetic Instructions - Add to Minus One Extend
    Page 71 of IBM Power ISATM Version 3.0 B
    examples:
    7c 22 01 d4  addme  r1,r2
    7c 22 01 d5  addme. r1,r2  *)
let addme cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let tm = signed var doubleword in
  RTL.[
    tm := ra;
    rt := ra + cpu.ca - one;
    cpu.ca := low cpu.word_width rt < low cpu.word_width tm;
    cpu.ca32 := low word rt < low word tm;
  ]

(** Fixed-Point Arithmetic Instructions - Add to Zero extended
    Page 72 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 01 94   addze r1,r2
    7c 22 01 95   addze. r1,r2 *)
let addze cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let tm = signed var doubleword in
  RTL.[
    tm := ra;
    rt := ra + cpu.ca;
    cpu.ca := low cpu.word_width rt < low cpu.word_width tm;
    cpu.ca32 := low word rt < low word tm;
  ]

let () =
  "ADD4"   >> add;
  "ADD4o"  >. add;
  "ADDI"   >> addi;
  "ADDIS"  >> addis;
  "ADDIC"  >> addic;
  "ADDICo" >. addic;
  "ADDC"   >> addc;
  "ADDCo"  >. addc;
  "ADDE"   >> adde;
  "ADDEo"  >. adde;
  "ADDME"  >> addme;
  "ADDMEo" >. addme;
  "ADDZE"  >> addze;
  "ADDZEo" >. addze;
  "LI"     >> li;
  "LIS"    >> lis;
  "LA"     >> addi;
