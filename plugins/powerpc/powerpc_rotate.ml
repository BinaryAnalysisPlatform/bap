open Bap_powerpc.Std

(** Fix-point Rotate Left Word Immediate then AND with Mask
    Page 102 of IBM Power ISATM Version 3.0 B
    example:
    55 2a 18 a0     rlwinm  r10, r8, 3, 2, 16
    55 2a 18 a1     rlwinm. r10, r8, 3, 2, 16 *)
let rlwinm cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let mb = unsigned imm ops.(3) in
  let me = unsigned imm ops.(4) in
  let tmp   = unsigned var word in
  let start = unsigned var cpu.word_width in
  let stop  = unsigned var cpu.word_width in
  let mask  = unsigned var cpu.word_width in
  let w32 = unsigned const word 32 in
  let ones = unsigned const cpu.word_width (-1) in
  RTL.[
    if_ (width ra <> w32) [
      start := mb + w32;
      stop  := me + w32;
    ] [
      start := mb;
      stop  := me;
    ];
    mask := ones;
    if_ (mb <= me) [
      mask := (mask lsr start) land (mask lsl (width ra - stop - one));
    ] [
      mask := (mask lsl (width ra - stop)) lor (mask lsr (start + one));
    ];
    tmp := nth word rs 1;
    ra := (nth cpu.word_width ((tmp ^ tmp ^ tmp) lsl sh) 0) land mask;
  ]

(** Fix-point Rotate Left Word then AND with Mask
    Page 103 of IBM Power ISATM Version 3.0 B
    example:
    5d 2a 19 54     rlwnm  r10, r9, r3, 5, 10
    5d 2a 19 55     rlwnm. r10, r9, r3, 5, 10 *)
let rlwnm cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let mb = unsigned imm ops.(3) in
  let me = unsigned imm ops.(4) in
  let tmp   = unsigned var word in
  let start = unsigned var cpu.word_width in
  let stop  = unsigned var cpu.word_width in
  let mask  = unsigned var cpu.word_width in
  let w32  = unsigned const word 32 in
  let ones = unsigned const cpu.word_width (-1) in
  RTL.[
    if_ (width ra <> w32) [
      start := mb + w32;
      stop  := me + w32;
    ] [
      start := mb;
      stop  := me;
    ];
    mask := ones;
    if_ (mb <= me) [
      mask := (mask lsr start) land (mask lsl (width ra - stop - one));
    ] [
      mask := (mask lsl (width ra - stop)) lor (mask lsr (start + one));
    ];
    tmp := nth word rs 1;
    ra := (nth doubleword ((tmp ^ tmp ^ tmp) lsl (last rb 5)) 0) land mask;
  ]

(** Fix-point Rotate Left Word Immediate then Mask Insert
    Page 103 of IBM Power ISATM Version 3.0 B
    example:
    51 2a 19 54     rlwimi  r10, r9, 3, 5, 10
    51 2a 19 55     rlwimi. r10, r9, 3, 5, 10 *)
let rlwimi cpu ops =
  let ra = unsigned cpu.reg ops.(1) in
  let rs = unsigned cpu.reg ops.(2) in
  let sh = unsigned imm ops.(3) in
  let mb = unsigned imm ops.(4) in
  let me = unsigned imm ops.(5) in
  let tmp1  = unsigned var word in
  let tmp2  = unsigned var cpu.word_width in
  let start = unsigned var cpu.word_width in
  let stop  = unsigned var cpu.word_width in
  let mask  = unsigned var cpu.word_width in
  let w32  = unsigned const cpu.word_width 32 in
  let ones = unsigned const cpu.word_width (-1) in
  RTL.[
    if_ (width ra <> w32) [
      start := mb + w32;
      stop  := me + w32;
    ] [
      start := mb;
      stop  := me;
    ];
    mask := ones;
    if_ (mb <= me) [
      mask := (mask lsr start) land (mask lsl (width ra - stop - one));
    ] [
      mask := (mask lsl (width ra - stop)) lor (mask lsr (start + one));
    ];
    tmp1 := nth word rs 1;
    tmp2 := nth doubleword ((tmp1 ^ tmp1 ^ tmp1) lsl sh) 0;
    ra := (tmp2 land mask) lor (ra land (lnot mask));
  ]

(** Fix-point Rotate Left Doubleword Immediate then Clear Left
    Page 104 of IBM Power ISATM Version 3.0 B
    example:
    79 2a 7f e2     rldicl  r10, r9, 47, 63
    79 2a 7f e3     rldicl. r10, r9, 47, 63 *)
let rldicl cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let mb = unsigned imm ops.(3) in
  let mask = unsigned var doubleword in
  RTL.[
    mask := zero;
    mask := (lnot mask) lsr mb;
    ra := (nth doubleword ((rs ^ rs ^ rs) lsl sh) 0) land mask;
  ]

(** Fix-point Rotate Left Doubleword Immediate then Clear Right
    Page 104 of IBM Power ISATM Version 3.0 B
    example:
    79 2a 26 44     rldicr  r10, r9, 4, 25
    79 2a 26 45     rldicr. r10, r9, 4, 25 *)
let rldicr cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let me = unsigned imm ops.(3) in
  let mask  = unsigned var doubleword in
  let width = unsigned const doubleword 64 in
  RTL.[
    mask := zero;
    mask := (lnot mask) lsl (width - me - one);
    ra := (nth doubleword ((rs ^ rs ^ rs) lsl sh) 0) land mask;
  ]

(** Fix-point Rotate Left Doubleword Immediate then Clear
    Page 105 of IBM Power ISATM Version 3.0 B
    example:
    79 2a 26 48     rldic  r10, r9, 4, 25
    79 2a 26 49     rldic. r10, r9, 4, 25 *)
let rldic cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let mb = unsigned imm ops.(3) in
  let mask = unsigned var doubleword in
  let mask1 = unsigned var doubleword in
  let mask2 = unsigned var doubleword in
  let ones = unsigned const doubleword (-1) in
  RTL.[
    mask := ones;
    mask1 := mask lsr mb;
    mask2 := mask lsl sh;
    mask := mask1 land mask2;
    ra := (nth doubleword ((rs ^ rs ^ rs) lsl sh) 0) land mask;
  ]

(** Fix-point Rotate Left Doubleword then Clear Left
    Page 105 of IBM Power ISATM Version 3.0 B
    example:
    79 2a 5e 50     rldcl  r10, r9, r11, 25
    79 2a 5e 51     rldcl. r10, r9, r11, 25 *)
let rldcl cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let mb = unsigned imm ops.(3) in
  let mask = unsigned var doubleword in
  RTL.[
    mask := zero;
    mask := (lnot mask) lsr mb;
    ra := (nth doubleword ((rs ^ rs ^ rs) lsl (last rb 6)) 0) land mask;
  ]

(** Fix-point Rotate Left Doubleword then Clear Right
    Page 106 of IBM Power ISATM Version 3.0 B
    example:
    79 2a 5e 52     rldcr  r10, r9, r11, 25
    79 2a 5e 53     rldcr. r10, r9, r11, 25 *)
let rldcr cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let me = unsigned imm ops.(3) in
  let mask = unsigned var doubleword in
  let width = unsigned const doubleword 64 in
  RTL.[
    mask := zero;
    mask := (lnot mask) lsl (width - me - one);
    ra := (nth doubleword ((rs ^ rs ^ rs) lsl (last rb 6)) 0) land mask;
  ]

(** Fix-point Rotate Left Doubleword Immediate then Mask Insert
    Page 106 of IBM Power ISATM Version 3.0 B
    example:
    79 2a 26 4c     rldimi  r10, r9, 4, 25
    79 2a 26 4d     rldimi. r10, r9, 4, 25 *)
let rldimi cpu ops =
  let ra = unsigned cpu.reg ops.(1) in
  let rs = unsigned cpu.reg ops.(2) in
  let sh = unsigned imm ops.(3) in
  let mb = unsigned imm ops.(4) in
  let tmp   = unsigned var doubleword in
  let mask  = unsigned var doubleword in
  let mask1 = unsigned var doubleword in
  let mask2 = unsigned var doubleword in
  let ones = unsigned const doubleword (-1) in
  RTL.[
    mask := ones;
    mask1 := mask lsr mb;
    mask2 := mask lsl sh;
    mask := mask1 land mask2;
    tmp := nth doubleword ((rs ^ rs ^ rs) lsl sh) 0;
    ra := (tmp land mask) lor (ra land (lnot mask));
  ]

let () =
  "RLWINM"  >> rlwinm;
  "RLWNM"   >> rlwnm;
  "RLWIMI"  >> rlwimi;
  "RLDICL"  >> rldicl;
  "RLDICR"  >> rldicr;
  "RLDIC"   >> rldic;
  "RLDCL"   >> rldcl;
  "RLDCR"   >> rldcr;
  "RLDIMI"  >> rldimi;
  "RLWINMo" >. rlwinm;
  "RLWNMo"  >. rlwnm;
  "RLWIMIo" >. rlwimi;
  "RLDICLo" >. rldicl;
  "RLDICRo" >. rldicr;
  "RLDICo"  >. rldic;
  "RLDCLo"  >. rldcl;
  "RLDCRo"  >. rldcr;
  "RLDIMIo" >. rldimi;
