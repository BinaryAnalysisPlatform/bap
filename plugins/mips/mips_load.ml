open Mips.Std

(* LB rt, offset(base)
 * Load Byte, MIPS32
 * Page 243 *)
let lb cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) byte;
  ]

(* LBE rt, offset(base)
 * Load Byte EVA, MIPS32
 * Page 245 *)
(* Not sure what to do here *)
let lbe cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) byte;
  ]

(* LBU rt, offset(base)
 * Load Byte Unsigned, MIPS32
 * Page 246 *)
let lbu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) byte;
  ]

(* LBUE rt, offset(base)
 * Load Byte Unsigned EVA, MIPS32
 * Page 247 *)
let lbue cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) byte;
  ]

(* LD rt, offset(base)
 * Load Doubleword, MIPS64
 * Page 248 *)
let ld cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) doubleword;
  ]

(* TODO: LDL rt, offset(base) *)
(* TODO: LDR rt, offset(base) *)

(* LDPC rt, offset
 * Load Doubleword PC-relative, MIPS64 Release 6
 * Page 254 *)
let ldpc cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (cpu.cia + (off lsl unsigned const byte 3)) doubleword;
  ]

(* LH rt, offset(base)
 * Load Halfword, MIPS32
 * Page 258 *)
let lh cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) halfword;
  ]

(* LHE rt, offset(base)
 * Load Halfword EVA, MIPS32
 * Page 259 *)
let lhe cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) halfword;
  ]

(* LHU rt, offset(base)
 * Load Halfword Unsigned, MIPS32
 * Page 260 *)
let lhu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) halfword;
  ]

(* LHUE rt, offset(base)
 * Load Halfword Unsigned EVA, MIPS32
 * Page 261 *)
let lhue cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) halfword;
  ]

(* LL rt, offset(base)
 * Load Linked Word, MIPS32
 * Page 262 *)
let ll cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) word;
  ]

(* LLD rt, offset(base)
 * Load Linked Doubleword, MIPS64
 * Page 264 *)
let lld cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) doubleword;
  ]

(* LLE rt, offset(base)
 * Load Linked Word EVA, MIPS32
 * Page 264 *)
let lle cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) word;
  ]

(* TODO: LLX rt, offset(base) *)
(* TODO: LLDX rt, offset(base) *)
(* TODO: LLXE rt, offset(base) *)

(* LW rt, offset(base)
 * Load Word, MIPS32
 * Page 281 *)
let lw cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) word;
  ]

(* LWE rt, offset(base)
 * Load Word EVA, MIPS32
 * Page 285 *)
let lwe cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) word;
  ]

(* TODO: LWL rt, offset(base) *)
(* TODO: LWLE rt, offset(base) *)
(* TODO: LWR rt, offset(base) *)
(* TODO: LWRE rt, offset(base) *)

(* LWPC rt, offset(base)
 * Load Word PC-relative, MIPS32 Release 6
 * 293 *)
let lwpc cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (cpu.cia + (off lsl unsigned const byte 2)) word;
  ]

(* LWU rt, offset(base)
 * Load Word Unsigned, MIPS64
 * Page 301 *)
let lwu cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (base + off) word;
  ]

(* LWUPC rt, offset(base)
 * Load Word Unsigned PC-relative
 * Page 302 *)
let lwupc cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    rt := cpu.load (cpu.cia + (off lsl unsigned const byte 2)) word;
  ]

let () =
  "LB" >> lb;
  "LBE" >> lbe;
  "LBu" >> lbu;
  "LBue" >> lbue;
  "LD" >> ld;
  "LDPC" >> ldpc;
  "LH" >> lh;
  "LHE" >> lhe;
  "LHu" >> lhu;
  "LHue" >> lhue;
  "LL" >> ll;
  "LLD" >> lld;
  "LLE" >> lle;
  "LW" >> lw;
  "LWE" >> lwe;
  "LWPC" >> lwpc;
  "LWu" >> lwu;
  "LWupc" >> lwupc;


