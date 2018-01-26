open Mips.Std

(* AND rd, rs, rt *)
let mand cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs land rt;
    ]

(* ANDI rd, rs, imm *)
let mandi cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let im = unsigned imm ops.(2) in
    RTL.[
        rd := rs land im;
    ]

(* EXT rd, rs, p, s *)
(* INS rd, rs, p, s *)
(* NOP *)
let nop cpu ops = []

(* NOR rd, rs, rt *)
let nor cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := lnot (rs lor rt);
    ]

(* NOT rd, rs *)
let mnot cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    RTL.[
        rd := lnot rs;
    ]

(* OR rd, rs, rt *)
let mor cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs lor rt;
    ]

(* ORI rd, rs, imm *)
let mori cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let im = unsigned imm ops.(2) in
    RTL.[
        rd := rs lor im;
    ]

(* WSBH rd, rs *)
(* XOR rd, rs, rt *)
let mxor cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs lxor rt;
    ]

(* XORI rd, rs, imm *)
let mxori cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let im = unsigned imm ops.(2) in
    RTL.[
        rd := rs lxor im;
    ]

(* BITSWAP rd, rt *) (* DBITSWAP rd, rt  - 64bit instruction *)
let bitswap cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rt = unsigned cpu.reg ops.(1) in
    let cnt = unsigned var byte in
    let biti = unsigned var bit in
    RTL.[
        (* Reverse bits *)
        cnt := zero;
        foreach biti rt [
            rd := rd lor (((rt lsr cnt) land one)
                lsl (unsigned const byte 31 - cnt));
            cnt := cnt + one;
        ];
    ]

let () =
    "AND" >> mand;
    "ANDi" >> mandi;
    "NOP" >> nop;
    "NOR" >> nor;
    "NOT" >> mnot;
    "OR" >> mor;
    "ORi" >> mori;
    "XOR" >> mxor;
    "XORi" >> mxori;
    "BITSWAP" >> bitswap;

