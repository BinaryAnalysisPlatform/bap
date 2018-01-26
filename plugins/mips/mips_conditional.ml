open Mips.Std

(* MUL rd, rs, rt *)
let mul cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := rs * rt;
    ]


