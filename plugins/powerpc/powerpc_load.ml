open Bap_powerpc.Std

(** Fixed-point Load Byte/Halfword/Word and Zero
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    89 3c 00 14 - lbz r9, 20(r28)
    89 20 00 14 - lbz r9, 20(0)
    a1 3c 00 14 - lhz r9, 20(r28)
    83 eb ff fc - lwz r31, -4(r11) *)
let lbz cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[ rt := cpu.load (ra + im) byte; ]

let lhz cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[ rt := cpu.load (ra + im) halfword; ]

let lwz cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[ rt := cpu.load (ra + im) word; ]

(** Fixed-point Load Byte/Halfword/Word and Zero Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3d 50 ae   lbzx r9, r29, r10
    7d 3d 52 2e   lhzx r9, r29, r10
    7d 3d 50 2e   lwzx r9, r29, r10  *)
let lbzx cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[ rt := cpu.load (ra + rb) byte; ]

let lhzx cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[ rt := cpu.load (ra + rb) halfword; ]

let lwzx cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[ rt := cpu.load (ra + rb) word; ]

(** Fixed-point Load Byte/Halfword/Word and Zero with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    8d 3c 00 14  lbzu r9, 20(r28)
    a5 3c 00 14  lhzu r9, 20(r28)
    85 3f ff fc  lwzu r9, -4(r31)  *)
let lbzu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(2) in
  let ra = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + im) byte;
    ra := ra + im;
  ]

let lhzu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(2) in
  let ra = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + im) halfword;
    ra := ra + im;
  ]

let lwzu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(2) in
  let ra = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + im) word;
    ra := ra + im;
  ]

(** Fixed-point Load Byte/Halfword/Word and Zero with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3d 50 ee  lbzux r9, r29, r10
    7d 3d 52 6e  lhzux r9, r29, r10
    7d 3d 50 6e  lwzux r9, r29, r10  *)
let lbzux cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + rb) byte;
    ra := ra + rb;
  ]

let lhzux cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + rb) halfword;
    ra := ra + rb;
  ]

let lwzux cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + rb) word;
    ra := ra + rb;
  ]


(** Fixed-point Load Halfword Algebraic
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    a8 29 00 05    lha r1, 5(r9) *)
let lha cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[
    rt := cpu.load (ra + im) halfword;
  ]

(** Fixed-point Load Word Algebraic
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    eb eb 01 16    lwa r31, 276(r11)  *)
let lwa cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  let sh = signed const byte 2 in
  RTL.[
    rt := cpu.load (ra + (im lsl sh)) word;
  ]

(** Fixed-point Load Halfword/Word Algebraic Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 25 4a ae    lhax r1, r5, r9
    7c 25 4a aa    lwax r1, r5, r9  *)
let lhax cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    rt := cpu.load (ra + rb) halfword;
  ]

let lwax cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    rt := cpu.load (ra + rb) word;
  ]

(** Fixed-point Load Halfword Algebraic with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    ac 29 00 05    lhau r1, 5(r9) *)
let lhau cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (ra + im) halfword;
    ra := ra + im
  ]

(** Fixed-point Load Data/Word Algebraic with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 25 4a ee    lhaux r1, r5, r9
    7c 25 4a ea    lwaux r1, r5, r9 *)
let lhaux cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + rb) halfword;
    ra := ra + rb;
  ]

let lwaux cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + rb) word;
    ra := ra + rb;
  ]

(** Fixed-point Load Dobuleword
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    e8 29 00 08    ld r1, 8(r9) *)
let ld cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  let sh = unsigned const byte 2 in
  RTL.[
    rt := cpu.load (ra + (im lsl sh)) doubleword;
  ]

(** Fixed-point Load Dobuleword Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 28 48 2a    ldx r1, r8, r9 *)
let ldx cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    rt := cpu.load (ra + rb) doubleword;
  ]

(** Fixed-point Load Dobuleword with Update
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    e8 29 00 09    ldu r1, 8(r9) *)
let ldu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  let sh = unsigned const byte 2 in
  RTL.[
    rt := cpu.load (ra + (im lsl sh)) doubleword;
    ra := ra + (im lsl sh);
  ]

(** Fixed-point Load Dobuleword with Update Indexed
    Pages 48-54 of IBM Power ISATM Version 3.0 B
    examples:
    7c 28 48 6a    ldux r1, r8, r9 *)
let ldux cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    rt := cpu.load (ra + rb) doubleword;
    ra := ra + rb;
  ]

(** Fixed-point Load Halfword Byte-Reverse Indexed
    Pages 60-61 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1e 2c    lhbrx r1, r2, r3  *)
let lhbrx cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let x = unsigned var halfword in
  RTL.[
    x := cpu.load (ra + rb) halfword;
    rt := nth byte x 1 ^ nth byte x 0;
  ]

(** Fixed-point Load Word Byte-Reverse Indexed
    Pages 60-61 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1c 2c    lwbrx r1, r2, r3 *)
let lwbrx cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let x = unsigned var word in
  RTL.[
    x := cpu.load (ra + rb) word;
    rt := nth byte x 3 ^ nth byte x 2 ^ nth byte x 1 ^ nth byte x 0;
  ]

(** Fixed-point Load Doubleword Byte-Reverse Indexed
    Pages 60-61 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1c 28    ldbrx r1, r2, r3  *)
let ldbrx cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let x = unsigned var doubleword in
  RTL.[
    x := cpu.load (ra + rb) doubleword;
    rt :=
      nth byte x 7 ^ nth byte x 6 ^ nth byte x 5 ^ nth byte x 4 ^
      nth byte x 3 ^ nth byte x 2 ^ nth byte x 1 ^ nth byte x 0;
  ]

let () =
  "LBZ"   >> lbz;
  "LHZ"   >> lhz;
  "LWZ"   >> lwz;
  "LBZX"  >> lbzx;
  "LHZX"  >> lhzx;
  "LWZX"  >> lwzx;
  "LBZU"  >> lbzu;
  "LHZU"  >> lhzu;
  "LWZU"  >> lwzu;
  "LBZUX" >> lbzux;
  "LHZUX" >> lhzux;
  "LWZUX" >> lwzux;
  "LHA"   >> lha;
  "LWA"   >> lwa;
  "LHAX"  >> lhax;
  "LWAX"  >> lwax;
  "LHAU"  >> lhau;
  "LHAUX" >> lhaux;
  "LWAUX" >> lwaux;
  "LD"    >> ld;
  "LDX"   >> ldx;
  "LDU"   >> ldu;
  "LDUX"  >> ldux;
  "LHBRX" >> lhbrx;
  "LWBRX" >> lwbrx;
  "LDBRX" >> ldbrx;
