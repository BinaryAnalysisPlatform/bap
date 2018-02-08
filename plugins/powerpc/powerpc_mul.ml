open Powerpc.Std

(** Fixed-Point Arithmetic Instructions - Multiply Low Immediate
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    1c 22 00 2a    mulli r1, r2, 42 *)
let mulli cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let si = signed imm ops.(2) in
  RTL.[
    rt := ra *  si;
  ]

(** Fixed-Point Arithmetic Instructions - Multiply High Word
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 96    mulhw  r1, r2, r3
    7c 22 18 97    mulhw. r1, r2, r3 *)
let mulhw cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let tmp1 = signed var doubleword in
  let tmp2 = signed var doubleword in
  RTL.[
    tmp1 := low word ra;
    tmp2 := low word rb;
    rt := high word (tmp1 * tmp2);
  ]

(** Fixed-Point Arithmetic Instructions - Multiply High Word Unsigned
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 16    mulhwu  r1, r2, r3
    7c 22 18 17    mulhwu. r1, r2, r3 *)
let mulhwu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  RTL.[
    tmp1 := low word ra;
    tmp2 := low word rb;
    rt := high word (tmp1 * tmp2);
  ]

(** Fixed-Point Arithmetic Instructions - Multiply Low Word
    Page 73 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 d6   mullw  r1, r2, r3
    7c 22 19 d7   mullw. r1, r2, r3 *)
let mullw cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let tmp1 = unsigned var doubleword in
  let tmp2 = unsigned var doubleword in
  RTL.[
    tmp1 := low word ra;
    tmp2 := low word rb;
    rt := tmp1 * tmp2;
  ]


(** Fixed-Point Arithmetic Instructions - Multiply low doubleword
    Page 79 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 19 d2   mulld  r1, r2, r3
    7c 22 19 d3   mulld. r1, r2, r3 *)
let mulld cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    rt := ra * rb;
  ]

(** Fixed-Point Arithmetic Instructions - Multiply high doubleword
    Page 79 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 92   mulhd  r1, r2, r3
    7c 22 18 93   mulhd. r1, r2, r3 *)
let mulhd cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let tm = signed var quadword in
  RTL.[
    tm := ra;
    tm := tm * rb;
    rt := high doubleword tm;
  ]

(** Fixed-Point Arithmetic Instructions - Multiply high doubleword unsigned
    Page 79 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 18 12   mulhdu  r1, r2, r3
    7c 22 18 13   mulhdu. r1, r2, r3 *)
let mulhdu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let tm = unsigned var quadword in
  RTL.[
    tm := ra;
    tm := tm * rb;
    rt := high doubleword tm;
  ]

let init () =
  "MULLI"   >| mulli;
  "MULHW"   >| mulhw;
  "MULHWo"  >. mulhw;
  "MULHWU"  >| mulhwu;
  "MULHWUo" >. mulhwu;
  "MULLW"   >| mullw;
  "MULLWo"  >. mullw;
  "MULLD"   >| mulld;
  "MULLDo"  >. mulld;
  "MULHD"   >| mulhd;
  "MULHDo"  >. mulhd;
  "MULHDU"  >| mulhdu;
  "MULHDUo" >. mulhdu;
