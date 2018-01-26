open Mips.Std

(* LB rt, offset(base) *)
let lb cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) byte;
    ]

(* LBE rt, offset(base) *) (* Not sure what do here *)
let lbe cpu ops =
    let rt = unsigned cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) byte;
    ]

(* LBU rt, offset(base) *)
let lbu cpu ops =
    let rt = unsigned cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) byte;
    ]

(* LBUE rt, offset(base) *)
let lbue cpu ops =
    let rt = unsigned cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) byte;
    ]

(* LD rt, offset(base) *)
let ld cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) doubleword;
    ]

(* LDPC rt, offset *)
let ldpc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (cpu.cia + (off lsl unsigned const byte 3)) doubleword;
    ]

(* LH rt, offset(base) *)
let lh cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) halfword;
    ]

(* LHE rt, offset(base) *)
let lhe cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) halfword;
    ]

(* LHU rt, offset(base) *)
let lhu cpu ops =
    let rt = unsigned cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) halfword;
    ]

(* LHUE rt, offset(base) *)
let lhue cpu ops =
    let rt = unsigned cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) halfword;
    ]

(* LL rt, offset(base) *)
let ll cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) word;
    ]

(* LLD rt, offset(base) *)
let lld cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) doubleword;
    ]

(* LLE rt, offset(base) *)
let lle cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) word;
    ]

(* LW rt, offset(base) *)
let lw cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) word;
    ]

(* LWE rt, offset(base) *)
let lwe cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) word;
    ]

(* LWPC rt, offset(base) *)
let lwpc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (cpu.cia + (off lsl unsigned const byte 2)) word;
    ]

(* LWU rt, offset(base) *)
let lwu cpu ops =
    let rt = unsigned cpu.reg ops.(0) in
    let base = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        rt := cpu.load (base + off) word;
    ]

(* LWUPC rt, offset(base) *)
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


