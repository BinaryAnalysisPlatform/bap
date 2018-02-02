open Mips.Std

(* SB rt, offset(base)
 * Store Byte, MIPS32
 * Page 396 *)
let sb cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt byte;
  ]

(* SBE rt, offset(base)
 * Store Byte EVA, MIPS32
 * Page 397 *)
let sbe cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt byte;
  ]

(* SC rt, offset(base)
 * Store Conditional Word, MIPS32
 * Page 398 *)
let sc cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt word;
  ]

(* SCD rt, offset(base)
 * Store Conditional Doubleword, MIPS64
 * Page 401 *)
let scd cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt doubleword;
  ]

(* SCE rt, offset(base)
 * Store Conditional Word EVA, MIPS32
 * Page 404 (for real) *)
let sce cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt word;
  ]

(* SD rt, offset(base)
 * Store Doubleword, MIPS64
 * Page 417 *)
let sd cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt doubleword;
  ]

(* TODO: SDL rt, offset(base) *)
(* TODO: SDR rt, offset(base) *)

(* SH rt, offset(base)
 * Store Halfword, MIPS32
 * Page 436 *)
let sh cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt halfword;
  ]

(* SHE rt, offset(base)
 * Store Halfword EVA, MIPS32
 * Page 437 *)
let she cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt halfword;
  ]

(* SW rt, offset(base)
 * Store Word, MIPS32
 * Page 456 *)
let sw cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt word;
  ]

(* SWE rt, offset(base)
 * Store Word EVA, MIPS32
 * Page 459 *)
let swe cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let base = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    cpu.store (base + off) rt word;
  ]

(* TODO: SWL rt, offset(base) *)
(* TODO: SWLE rt, offset(base) *)
(* TODO: SWR rt, offset(base) *)
(* TODO: SWRE rt, offset(base) *)

let () =
  "SB" >> sb;
  "SBE" >> sbe;
  "SC" >> sc;
  "SCD" >> scd;
  "SCE" >> sce;
  "SD" >> sd;
  "SH" >> sh;
  "SHE" >> she;
  "SW" >> sw;
  "SWE" >> swe;
