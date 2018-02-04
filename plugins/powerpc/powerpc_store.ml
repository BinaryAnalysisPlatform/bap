open Bap_powerpc.Std

(** Fixed-point Store Byte/Halfword/Word
    Pages 54-56 of IBM Power ISATM Version 3.0 B
    examples:
    99 3c 01 6c     stb r9,364(r28)
    99 20 01 6C     stb r9,364(0)
    b1 3c 01 6c     sth r9,364(r28)
    91 28 ff d4     stw r9,-44(r8) *)
let stb cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[ cpu.store (ra + im) rs byte; ]

let sth cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[ cpu.store (ra + im) rs halfword; ]

let stw cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[ cpu.store (ra + im) rs word; ]

(** Fixed-point Store Byte/Halfword/Word/Doubleword Indexed
    Pages 54-57 of IBM Power ISATM Version 3.0 B
    examples:
    7d 2e f9 ae     stbx    r9,r14,r31
    7d 3e eb 2e     sthx    r9,r30,r29
    7f b6 f9 2e     stwx    r29,r22,r31
    7c 28 49 2a     stdx    r1, r8, r9  *)
let stbx cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs byte; ]

let sthx cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs halfword; ]

let stwx cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs word; ]

let stdx cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[ cpu.store (ra + rb) rs doubleword; ]

(** fixed-point Store Byte/Halfword/Word with Update
    Pages 54-56 of IBM Power ISATM Version 3.0 B
    examples:
    9c 9d ff ff     stbu r4,-1(r29)
    b5 3d ff ff     sthu r9,-1(r29)
    94 21 ff f0     stwu r1,-16(r1)  *)
let stbu cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed cpu.reg ops.(3) in
  RTL.[
    cpu.store (ra + im) rs byte;
    ra := ra + im;
  ]

let sthu cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed cpu.reg ops.(3) in
  RTL.[
    cpu.store (ra + im) rs halfword;
    ra := ra + im;
  ]

let stwu cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed cpu.reg ops.(3) in
  RTL.[
    cpu.store (ra + im) rs word;
    ra := ra + im;
  ]

(** Fixed-point Store Byte/Halfword/Word/Doubleword with Update Indexed
    Pages 54-57 of IBM Power ISATM Version 3.0 B
    examples:
    7d 3f c9 ee     stbux r9,r31,r25
    7d 3f cb 6e     sthux r9,r31,r25
    7d 41 49 6e     stwux r10,r1,r9
    7c 28 49 6a     stdux r1,r8,r9   *)
let stbux cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs byte;
    ra := ra + rb;
  ]

let sthux cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs halfword;
    ra := ra + rb;
  ]

let stwux cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs word;
    ra := ra + rb;
  ]

let stdux cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  let rb = signed cpu.reg ops.(3) in
  RTL.[
    cpu.store (ra + rb) rs doubleword;
    ra := ra + rb;
  ]

(** Fixed-point Store Doubleword
    Page 57 of IBM Power ISATM Version 3.0 B
    examples:
    f8 29 00 08   std r1, 8(r9) *)
let std cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let im = signed imm ops.(1) in
  let ra = signed cpu.reg ops.(2) in
  RTL.[
    cpu.store (ra + im) rs doubleword;
  ]

(** Fixed-point Store Doubleword with Update
    Page 57 of IBM Power ISATM Version 3.0 B
    examples:
    f8 29 00 09   stdu r1, 8(r9) *)
let stdu cpu ops =
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  let ra = signed cpu.reg ops.(3) in
  let ea = unsigned var doubleword in
  RTL.[
    ea := ra + im;
    cpu.store ea rs doubleword;
    ra := ea;
  ]

(** Fixed-point Store Halfword Byte-Reverse Indexed
    Pages 60-61 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1f 2c    sthbrx r1, r2, r3  *)
let sthbrx cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let ea = unsigned var cpu.word_width in
  let x = unsigned var halfword in
  let y = unsigned var doubleword in
  RTL.[
    ea := ra + rb;
    y := rs;
    x := nth byte y 7 ^ nth byte y 6;
    cpu.store ea x halfword;
  ]

(** Fixed-point Store Word Byte-Reverse Indexed
    Pages 60-61 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1d 2c    stwbrx r1, r2, r3  *)
let stwbrx cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let ea = unsigned var cpu.word_width in
  let x = unsigned var word in
  let y = unsigned var doubleword in
  RTL.[
    ea := ra + rb;
    y := rs;
    x := nth byte y 7 ^ nth byte y 6 ^ nth byte y 5 ^ nth byte y 4;
    cpu.store ea x word;
  ]

(** Fixed-point Store Doubleword Byte-Reverse Indexed
    Pages 60-61 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1d 28    stdbrx r1, r2, r3  *)
let stdbrx cpu ops =
  let rs = unsigned cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let ea = unsigned var doubleword in
  let x = unsigned var doubleword in
  RTL.[
    ea := ra + rb;
    x :=
      nth byte rs 7 ^ nth byte rs 6 ^ nth byte rs 5 ^ nth byte rs 4 ^
      nth byte rs 3 ^ nth byte rs 2 ^ nth byte rs 1 ^ nth byte rs 0;
    cpu.store ea x doubleword;
  ]

let () =
  "STB"    >> stb;
  "STH"    >> sth;
  "STW"    >> stw;
  "STBX"   >> stbx;
  "STHX"   >> sthx;
  "STWX"   >> stwx;
  "STDX"   >> stdx;
  "STBU"   >> stbu;
  "STHU"   >> sthu;
  "STWU"   >> stwu;
  "STBUX"  >> stbux;
  "STHUX"  >> sthux;
  "STWUX"  >> stwux;
  "STDUX"  >> stdux;
  "STD"    >> std;
  "STDU"   >> stdu;
  "STHBRX" >> sthbrx;
  "STWBRX" >> stwbrx;
  "STDBRX" >> stdbrx;
