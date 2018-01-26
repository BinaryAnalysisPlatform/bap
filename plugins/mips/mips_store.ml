open Mips.Std

(* SB rt, offset(base) *)
let sb cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt byte;
    ]

(* SBE rt, offset(base) *)
let sbe cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt byte;
    ]

(* SC rt, offset(base) *)
let sc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt word;
    ]

(* SCD rt, offset(base) *)
let scd cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt doubleword;
    ]

(* SCE rt, offset(base) *)
let sce cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt word;
    ]

(* SD rt, offset(base) *)
let sd cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt doubleword;
    ]

(* SH rt, offset(base) *)
let sh cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt halfword;
    ]

(* SHE rt, offset(base) *)
let she cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt halfword;
    ]

(* SW rt, offset(base) *)
let sw cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt word;
    ]

(* SWE rt, offset(base) *)
let swe cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        cpu.store (base + off) rt word;
    ]

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
