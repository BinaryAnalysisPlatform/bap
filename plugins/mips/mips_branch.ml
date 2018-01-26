open Mips.Std

(* BAL rs, offset *)
let bal cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        rs := cpu.cia + unsigned const byte 8;
        cpu.jmp (cpu.cia + off);
    ]

(* BEQ rs, rt, offset *)
let beq cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs = rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BEQL rs, rt, offset *)
let beql cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs = rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGEZ rs, offset *)
let bgez cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs >= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLEZ rs, offset *)
let blez cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs <= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]
(* BGEZAL rs, offset *)
let bgezal cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        (* )cpu.lr := cpu.cia + unsigned const byte 8; *)
        when_ (rs >= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLTZALC rt, offset *)
let bltzalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt < zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BLEZALC rt, offset *)
let blezalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt <= zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BGEZALC rt, offset *)
let bgezalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt >= zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BGTZALC rt, offset *)
let bgtzalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt > zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BEQZALC rt, offset *)
let beqzalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt = zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BNEZALC rt, offset *)
let bnezalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt <> zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BLTC rs, rt, offset *)
let bltc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs < rt) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BGEC rs, rt, offset *)
let bgec cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs >= rt) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BEQC rs, rt, offset *)
let beqc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs = rt) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BNEC rs, rt, offset *)
let bnec cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs <> rt) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BLTUC rs, rt, offset *)
let bltuc cpu ops =
    let rs = unsigned cpu.reg ops.(0) in
    let rt = unsigned cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs < rt) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BGEUC rs, rt, offset *)
let bgeuc cpu ops =
    let rs = unsigned cpu.reg ops.(0) in
    let rt = unsigned cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs >= rt) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BLTZC rt, offset *)
let bltzc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt < zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BLEZC rt, offset *)
let blezc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt <= zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BGEZC rt, offset *)
let bgezc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt >= zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BGTZC rt, offset *)
let bgtzc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt > zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BEQZC rs, offset *)
let beqzc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs = zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BNEZC rs, offset *)
let bnezc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs <> zero) [
            cpu.jmp (cpu.cia + off + unsigned const byte 4);
        ];
    ]

(* BGEZL rs, offset *)
let bgezc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs >= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGTZ rs, offset *)
let bgtz cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs >= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGTZL rs, offset *)
let bgtzl cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs >= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* J target *)
let jump cpu ops =
    let target = signed imm ops.(0) in
    RTL.[
        cpu.jmp target;
    ]

(* JAL target *)
let jal cpu ops =
    let target = signed imm ops.(0) in
    RTL.[
        cpu.cia := cpu.cia + unsigned const byte 8;
        cpu.jmp target;
    ]

(* JALR rd, rs *)
let jalr cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    RTL.[
        rd := cpu.cia + unsigned const byte 8;
        cpu.jmp rs;
    ]

(* JIALC rt, offset *)
let jialc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        cpu.cia := cpu.cia + unsigned const byte 4;
        cpu.jmp (rt + off);
    ]

(* JIC rt, offset *)
let jic cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        cpu.jmp (rt + off);
    ]

(* JR rs *)
let jr cpu ops =
    let rs = signed cpu.reg ops.(0) in
    RTL.[
        cpu.jmp rs;
    ]

let () =
    "BAL" >> bal;
    "BEQ" >> beq;
    "BEQL" >> beql;
    "BGEZ" >> bgez;
    "BLEZ" >> blez;
    "BGEZAL" >> bgezal;
    "BLTZALC" >> bltzalc;
    "BLEZALC" >> blezalc;
    "BGEZALC" >> bgezalc;
    "BGTZALC" >> bgtzalc;
    "BEQZALC" >> beqzalc;
    "BNEZALC" >> bnezalc;
    "BLTC" >> bltc;
    "BGEC" >> bgec;
    "BEQC" >> beqc;
    "BNEC" >> bnec;
    "BLTUC" >> bltuc;
    "BGEUC" >> bgeuc;
    "BLTZC" >> bltzc;
    "BLEZC" >> blezc;
    "BGEZC" >> bgezc;
    "BGTZC" >> bgtzc;
    "BEQZC" >> beqzc;
    "BNEZC" >> bnezc;
    "BGEZC" >> bgezc;
    "BGTZ" >> bgtz;
    "BGTZL" >> bgtzl;
    "J" >> jump;
    "JAL" >> jal;
    "JALR" >> jalr;
    "JIALC" >> jialc;
    "JIC" >> jic;
    "JR" >> jr;

