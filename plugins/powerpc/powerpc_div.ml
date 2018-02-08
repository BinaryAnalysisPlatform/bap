open Powerpc.Std


(** Fixed-Point Arithmetic Instructions - Divide Word
    Page 74 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b d6    divw  r1, r2, r3
    7c 22 1b d7    divw. r1, r2, r3 *)
let divw cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let dividend = signed var word in
  let divisor = signed var word in
  RTL.[
    dividend := low word ra;
    divisor := low word rb;
    rt := dividend / divisor;
  ]

(** Fixed-Point Arithmetic Instructions - Divide Word Unsigned
    Page 74 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 96    divwu  r1, r2, r3
    7c 22 1b 97    divwu. r1, r2, r3 *)
let divwu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    rt := low word ra / low word rb;
  ]

(** Fixed-Point Arithmetic Instructions - Divide Word Extended
    Page 75 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 56    divwe  r1, r2, r3
    7c 22 1b 57    divwe. r1, r2, r3 *)
let divwe cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let x = signed var doubleword in
  RTL.[
    x := zero;
    high word x := low word ra;
    rt := low word (x / low word rb);
  ]

(** Fixed-Point Arithmetic Instructions - Divide Word Extended Unsigned
    Page 75 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 16    divweu  r1, r2, r3
    7c 22 1b 17    divweu. r1, r2, r3 *)
let divweu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let x = unsigned var doubleword in
  RTL.[
    x := zero;
    high word x := low word ra;
    rt := low word (x / low word rb);
  ]

(** Fixed-Point Arithmetic Instructions - Modulo signed word
    Page 77 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1e 16   modsw r1, r2, r3 *)
let modsw cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let dividend = signed var word in
  let divisor = signed var word in
  RTL.[
    dividend := low word ra;
    divisor := low word rb;
    rt := dividend % divisor;
  ]

(** Fixed-Point Arithmetic Instructions - Modulo unsigned word
    Page 77 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1a 16   moduw  r1, r2, r3
    7c 22 1a 17   moduw. r1, r2, r3 *)
let moduw cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    rt := low word ra % low word rb;
  ]

(** Fixed-Point Arithmetic Instructions - Divide doubleword
    Page 81 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b d2   divd  r1, r2, r3
    7c 22 1b d3   divd. r1, r2, r3 *)
let divd cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    rt := ra /  rb;
  ]

(** Fixed-Point Arithmetic Instructions - Divide doubleword unsigned
    Page 81 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 92   divdu  r1, r2, r3
    7c 22 1b 93   divdu. r1, r2, r3 *)
let divdu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    rt := ra / rb;
  ]

(** Fixed-Point Arithmetic Instructions - Divide doubleword extended
    Page 82 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 52   divde  r1, r2, r3
    7c 22 1b 53   divde. r1, r2, r3 *)
let divde cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  let tm1 = signed var quadword in
  let tm2 = signed var quadword in
  RTL.[
    tm1 := zero;
    high doubleword tm1 := ra;
    tm2 := tm1 / rb;
    rt := low doubleword tm2;
  ]

(** Fixed-Point Arithmetic Instructions - Divide doubleword extended unsigned
    Page 82 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1b 12   divdeu  r1, r2, r3
    7c 22 1b 13   divdeu. r1, r2, r3 *)
let divdeu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  let tm1 = unsigned var quadword in
  let tm2 = unsigned var quadword in
  RTL.[
    tm1 := zero;
    high doubleword tm1 := ra;
    tm2 := tm1 / rb;
    rt := low doubleword tm2;
  ]

(** Fixed-Point Arithmetic Instructions - Modulo signed doubleword
    Page 83 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1e 12   modsd  r1, r2, r3 *)
let modsd cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let ra = signed cpu.reg ops.(1) in
  let rb = signed cpu.reg ops.(2) in
  RTL.[
    rt := ra % rb;
  ]

(** Fixed-Point Arithmetic Instructions - Modulo unsigned doubleword
    Page 83 of IBM Power ISATM Version 3.0 B
    example:
    7c 22 1a 12  modud  r1, r2, r3 *)
let modud cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let ra = unsigned cpu.reg ops.(1) in
  let rb = unsigned cpu.reg ops.(2) in
  RTL.[
    rt := ra % rb;
  ]

let () =
  "DIVW"    >| divw;
  "DIVWo"   >. divw;
  "DIVWU"   >| divwu;
  "DIVWUo"  >. divwu;
  "DIVWE"   >| divwe;
  "DIVWEo"  >. divwe;
  "DIVWEU"  >| divweu;
  "DIVWEUo" >. divweu;
  "MODSW"   >| modsw;
  "MODUW"   >| moduw;
  "DIVD"    >| divd;
  "DIVDo"   >. divd;
  "DIVDU"   >| divdu;
  "DIVDUo"  >. divdu;
  "DIVDE"   >| divde;
  "DIVDEo"  >. divde;
  "DIVDEU"  >| divdeu;
  "DIVDEUo" >. divdeu;
  "MODSD"   >| modsd;
  "MODUD"   >| modud;
