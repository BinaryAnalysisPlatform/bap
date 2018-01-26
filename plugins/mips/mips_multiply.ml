open Mips.Std

(* MUL rd, rs, rt *)
let mul cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

(* MUH rd, rs, rt *)
let muh cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

(* MULU rd, rs, rt *)
let mul cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

(* MUHU rd, rs, rt *)
let muhu cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

(* DMUL rd, rs, rt *)
let dmul cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

(* DMUH rd, rs, rt *)
let dmuh cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

(* DMULU rd, rs, rt *)
let mul cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

(* DMUH rd, rs, rt *)
let dmuhu cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]

