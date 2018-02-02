open Mips.Std

(* MUL rd, rs, rt
 * Multiply Words Signed, Low Word, MIPS32
 * Page 351 *)
let mul cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := low word (rs * rt);
    ]

(* MUH rd, rs, rt
 * Multiply Words Signed, High Word, MIPS32
 * Page 351 *)
let muh cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := high word (rs * rt);
    ]

(* MULU rd, rs, rt
 * Multiply Words Unsigned, Low Word, MIPS32
 * Page 351 *)
let mulu cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := low word (rs * rt);
    ]

(* MUHU rd, rs, rt
 * Multiply Words Unsigned, High Word, MIPS32
 * Page 351 *)
let muhu cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := high word (rs * rt);
    ]

(* DMUL rd, rs, rt
 * Multiply Doublewords Signed, Low Doubleword, MIPS64
 * Page 351 *)
let dmul cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := low doubleword (rs * rt);
    ]

(* DMUH rd, rs, rt
 * Multiply Doublewords Signed, High Doubleword, MIPS64
 * Page 351 *)
let dmuh cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let rt = signed cpu.reg ops.(2) in
    RTL.[
        rd := high doubleword (rs * rt);
    ]

(* DMULU rd, rs, rt
 * Multiply Doublewords Unsigned, Low Doubleword, MIPS64
 * Page 351 *)
let dmulu cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := low doubleword (rs * rt);
    ]

(* DMUHU rd, rs, rt
 * Multiply Doublewords Unsigned, High Doubleword, MIPS64
 * Page 351 *)
let dmuhu cpu ops =
    let rd = unsigned cpu.reg ops.(0) in
    let rs = unsigned cpu.reg ops.(1) in
    let rt = unsigned cpu.reg ops.(2) in
    RTL.[
        rd := high doubleword (rs * rt);
    ]

(* MULT rs, rt
 * Multiply Signed Word MIPS32, removed in Release 6
 * Page 354 *)
let mult cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let x = signed var doubleword in
    RTL.[
        x := rs * rt;
        cpu.hi := high word x;
        cpu.lo := low word x;
    ]

(* MULTU rs, rt
 * Multiply Unsigned Word MIPS32, removed in Release 6
 * Page 356 *)
let multu cpu ops =
    let rs = unsigned cpu.reg ops.(0) in
    let rt = unsigned cpu.reg ops.(1) in
    let x = unsigned var doubleword in
    RTL.[
        x := rs * rt;
        cpu.hi := high word x;
        cpu.lo := low word x;
    ]

let () =
    "MUL" >> mul;
    "MUH" >> muh;
    "MULu" >> mulu;
    "MUHu" >> muhu;
    "DMUL" >> dmul;
    "DMUH" >> dmuh;
    "DMULu" >> dmulu;
    "DMUHu" >> dmuhu;
    "MULT" >> mult;
    "MULTU" >> multu;
