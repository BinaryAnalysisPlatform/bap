open Powerpc.Std

(** Fix-point Shift Left Word
    Page 107 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 58 30     slw  r10, r9, r11
    7d 2a 58 31     slw. r10, r9, r11  *)
let slw cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let sh = unsigned const byte 5 in
  RTL.[
    if_ (lsb (rb >> sh) = zero) [
      ra := low word rs << last rb 5;
    ] [
      ra := zero;
    ]
  ]

(** Fix-point Shift Right Word
    Page 107 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5c 30     srw  r10, r9, r11
    7d 2a 5c 31     srw. r10, r9, r11 *)
let srw cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let sh = unsigned const byte 5 in
  RTL.[
    if_ (lsb (rb >> sh) = zero) [
      ra := low word rs >> last rb 5;
    ] [
      ra := zero;
    ]
  ]

(** Fix-point Shift Right Algebraic Word Immediate
    Page 108 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5e 70     srawi  r10, r9, 11
    7d 2a 5e 71     srawi. r10, r9, 11 *)
let srawi cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let mask = unsigned var cpu.word_width in
  let carry_ones = unsigned var bit in
  let ones = unsigned const cpu.word_width (-1) in
  let w32 = unsigned const byte 32 in
  let tm = signed var word in
  RTL.[
    mask := ones;
    carry_ones := ((lnot (mask << sh)) land rs) <> zero;
    tm := low word rs;
    cpu.ca := carry_ones land (tm < zero);
    cpu.ca32 := cpu.ca;
    if_ (tm >= zero) [
      ra := low word rs >> sh;
    ] [
      if_ (width ra = w32) [
        mask := mask >> sh;
      ] [
        mask := mask >> (w32 + sh);
      ];
      ra := (low word rs >> sh) lor (lnot mask);
    ]
  ]

(** Fix-point Shift Right Algebraic Word
    Page 108 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5e 30     sraw  r10, r9, r11
    7d 2a 5e 31     sraw. r10, r9, r11 *)
let sraw cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let mask = unsigned var cpu.word_width in
  let w32 = unsigned const cpu.word_width 32 in
  let carry_ones = unsigned var bit in
  let s = unsigned var bit in
  let shift = unsigned var byte in
  let tm = signed var word in
  RTL.[
    mask := zero;
    shift := last rb 6;
    when_ (last rb 6 < w32) [
      mask := (lnot mask) << shift;
    ];
    tm := low word rs;
    s := tm < zero;
    carry_ones := ((lnot mask) land rs) <> zero;
    cpu.ca   := carry_ones land s;
    cpu.ca32 := cpu.ca;
    if_ (s = zero) [
      ra := low word rs >> shift;
    ] [
      if_ (width ra = w32) [
        mask := mask >> shift;
      ] [
        mask := mask >> (w32 + shift);
      ];
      ra := (low word rs >> shift) lor (lnot mask);
    ]
  ]

(** Fix-point Shift Left Doubleword
    Page 109 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 58 36     sld  r10, r9, r11
    7d 2a 58 37     sld. r10, r9, r11 *)
let sld cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    ra := rs << (last rb 6)
  ]

(** Fix-point Shift Right Doubleword
    Page 109 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5c 36     srd  r10, r9, r11
    7d 2a 5c 37     srd. r10, r9, r11 *)
let srd cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    ra := rs >> (last rb 6)
  ]

(** Fix-point Shift Right Algebraic Doubleword Immediate
    Page 110 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 26 74     sradi  r10, r9, 4
    7d 2a 26 75     sradi. r10, r9, 4 *)
let sradi cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let sh = unsigned imm ops.(2) in
  let mask = unsigned var doubleword in
  let carry_ones = unsigned var bit in
  let tm = signed var cpu.word_width in
  RTL.[
    mask := zero;
    mask := lnot mask;
    carry_ones := ((lnot (mask << sh)) land rs) <> zero;
    tm := rs;
    cpu.ca   := carry_ones land (tm < zero);
    cpu.ca32 := cpu.ca;
    if_ (tm >= zero) [
      ra := rs >> sh;
    ] [
      mask := mask >> sh;
      ra := (rs >> sh) lor (lnot mask);
    ]
  ]

(** Fix-point Shift Right Algebraic Doubleword
    Page 110 of IBM Power ISATM Version 3.0 B
    example:
    7d 2a 5e 34     srad  r10, r9, r11
    7d 2a 5e 35     srad. r10, r9, r11 *)
let srad cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let mask = unsigned var doubleword in
  let carry_ones = unsigned var bit in
  let s = unsigned var bit in
  let shift = unsigned var byte in
  let tm = signed var cpu.word_width in
  RTL.[
    mask := zero;
    shift := last rb 7;
    when_ (nth bit rb 57 = zero) [
      mask := (lnot mask) << shift;
    ];
    tm := rs;
    s := tm < zero;
    carry_ones := ((lnot mask) land rs) <> zero;
    cpu.ca   := carry_ones land s;
    cpu.ca32 := cpu.ca;
    if_ (s = zero) [
      ra := rs >> shift;
    ] [
      mask := mask >> shift;
      ra := (rs >> shift) lor (lnot mask);
    ]
  ]

let init () =
  "SLW"    >| slw;
  "SRW"    >| srw;
  "SRAWI"  >| srawi;
  "SRAW"   >| sraw;
  "SLD"    >| sld;
  "SRD"    >| srd;
  "SRADI"  >| sradi;
  "SRAD"   >| srad;
  "SLWo"   >. slw;
  "SRWo"   >. srw;
  "SRAWIo" >. srawi;
  "SRAWo"  >. sraw;
  "SLDo"   >. sld;
  "SRDo"   >. srd;
  "SRADIo" >. sradi;
  "SRADo"  >. srad;
