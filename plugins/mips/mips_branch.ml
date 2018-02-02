open Mips.Std

(* BAL rs, offset *)
let bal cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    let step = unsigned const byte 2 in
    let byte = unsigned const byte 8 in
    RTL.[
        rs := cpu.cia + step * cpu.word_width / byte;
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
    let step = unsigned const byte 2 in
    let byte = unsigned const byte 8 in
    RTL.[
        when_ (rs >= zero) [
            cpu.gpr 31 := cpu.cia + step * cpu.word_width / byte;
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLTZALC rt, offset *)
let bltzalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt < zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLEZALC rt, offset *)
let blezalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt <= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGEZALC rt, offset *)
let bgezalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt >= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGTZALC rt, offset *)
let bgtzalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt > zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BEQZALC rt, offset *)
let beqzalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt = zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BNEZALC rt, offset *)
let bnezalc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt <> zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLTC rs, rt, offset *)
let bltc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs < rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGEC rs, rt, offset *)
let bgec cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs >= rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BEQC rs, rt, offset *)
let beqc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs = rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BNEC rs, rt, offset *)
let bnec cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let rt = signed cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs <> rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLTUC rs, rt, offset *)
let bltuc cpu ops =
    let rs = unsigned cpu.reg ops.(0) in
    let rt = unsigned cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs < rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGEUC rs, rt, offset *)
let bgeuc cpu ops =
    let rs = unsigned cpu.reg ops.(0) in
    let rt = unsigned cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (rs >= rt) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLTZC rt, offset *)
let bltzc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt < zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BLEZC rt, offset *)
let blezc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt <= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGEZC rt, offset *)
let bgezc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt >= zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BGTZC rt, offset *)
let bgtzc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rt > zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BEQZC rs, offset *)
let beqzc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs = zero) [
            cpu.jmp (cpu.cia + off);
        ];
    ]

(* BNEZC rs, offset *)
let bnezc cpu ops =
    let rs = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    RTL.[
        when_ (rs <> zero) [
            cpu.jmp (cpu.cia + off);
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
    let step = unsigned const byte 2 in
    let byte = unsigned const byte 8 in
    RTL.[
        cpu.gpr 31 := cpu.cia + step * cpu.word_width / byte;
        cpu.jmp target;
    ]

(* JALR rd, rs *)
let jalr cpu ops =
    let rd = signed cpu.reg ops.(0) in
    let rs = signed cpu.reg ops.(1) in
    let step = unsigned const byte 2 in
    let byte = unsigned const byte 8 in
    RTL.[
        rd := cpu.cia + step * cpu.word_width / byte;
        cpu.jmp rs;
    ]

(* JIALC rt, offset *)
let jialc cpu ops =
    let rt = signed cpu.reg ops.(0) in
    let off = signed imm ops.(1) in
    let step = unsigned const byte 2 in
    let byte = unsigned const byte 8 in
    RTL.[
        cpu.gpr 31 := cpu.cia + step * cpu.word_width / byte;
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

(* BNE ra, rb, offset  *)
let bne cpu ops =
    let ra = unsigned cpu.reg ops.(0) in
    let rb = unsigned cpu.reg ops.(1) in
    let off = signed imm ops.(2) in
    RTL.[
        when_ (lnot (ra = rb)) [
            cpu.jmp (cpu.cia + off)
        ]
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
    "BNE" >> bne;


    (* SLT  v0,v1,v0 *)
let slt cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    if_ (rs <$ rt) [
      rd := one;
    ] [
      rd := zero
    ]
  ]

(* SLTU  v1,v0,s1 *)
let sltu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    if_ (rs < rt) [
      rd := one;
    ] [
      rd := zero
    ]
  ]

(* SLTiu  v0,v0,7 *)
let sltiu cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[
    if_ (rs < im) [
      rt := one;
    ] [
      rt := zero
    ]
  ]

let () =
  "SLT"   >> slt;
  "SLTu"  >> sltu;
  "SLTiu" >> sltiu
