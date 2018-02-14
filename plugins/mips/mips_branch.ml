open Mips.Std

(* BAL rs, offset
 * Branch and Link, MIPS32
 * Page 54 *)
let bal cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let off = signed imm ops.(1) in
  let step = unsigned const byte 8 in
  RTL.[
    rs := cpu.cia + step;
    cpu.jmp (cpu.cia + off);
  ]

(* BEQ rs, rt, offset
 * Branch on Equal, MIPS32
 * Page 76 *)
let beq cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let rt = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    when_ (rs = rt) [
      cpu.jmp (cpu.cia + off);
    ];
  ]

(* BEQL rs, rt, offset
 * Branch on Equal Likely, MIPS32, removed in Release 6
 * Page 77 *)
let beql cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let rt = signed cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    when_ (rs = rt) [
      cpu.jmp (cpu.cia + off);
    ];
  ]

(* BGEZ rs, offset
 * Branch on Greater Than or Equal to Zero, MIPS32
 * Page 79 *)
let bgez cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let off = signed imm ops.(1) in
  RTL.[
    when_ (rs >= zero) [
      cpu.jmp (cpu.cia + off);
    ];
  ]

(* BLEZ rs, offset
 * Branch on Less Than or Equal to Zero, MIPS32
 * Page 97 *)
let blez cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let off = signed imm ops.(1) in
  RTL.[
    when_ (rs <= zero) [
      cpu.jmp (cpu.cia + off);
    ];
  ]

(* BGEZAL rs, offset
 * Branch on Greater Than or Equal to Zero and Link, MIPS32, removed in Release 6
 * Page 80 *)
let bgezal cpu ops =
  let rs = signed cpu.reg ops.(0) in
  let off = signed imm ops.(1) in
  let step = unsigned const byte 8 in
  RTL.[
    when_ (rs >= zero) [
      cpu.gpr 31 := cpu.cia + step;
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

(* J target
 * Jump, MIPS32
 * Page 226 *)
let jump cpu ops =
  let target = signed imm ops.(0) in
  RTL.[
    cpu.jmp target;
  ]

(* JAL target
 * Jump and Link, MIPS32
 * Page 227 *)
let jal cpu ops =
  let target = signed imm ops.(0) in
  let step = unsigned const byte 8 in
  RTL.[
    cpu.gpr 31 := cpu.cia + step;
    cpu.jmp target;
  ]

(* JALR rd, rs
 * Jump and Link Register, MIPS32
 * Page 228 *)
let jalr cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let step = unsigned const byte 8 in
  RTL.[
    rd := cpu.cia + step;
    cpu.jmp rs;
  ]

(* JIALC rt, offset
 * Jump Indexed and Link, Compact, MIPS32 Release 6
 * Page 236 *)
let jialc cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let off = signed imm ops.(1) in
  let step = unsigned const byte 8 in
  RTL.[
    cpu.gpr 31 := cpu.cia + step;
    cpu.jmp (rt + off);
  ]

(* JIC rt, offset
 * Jump Indexed, Compact, MIPS32 Release 6
 * Page 238 *)
let jic cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let off = signed imm ops.(1) in
  RTL.[
    cpu.jmp (rt + off);
  ]

(* JR rs
 * Jump Register, MIPS32
 * Page 239 *)
let jr cpu ops =
  let rs = signed cpu.reg ops.(0) in
  RTL.[
    cpu.jmp rs;
  ]

(* BNE ra, rb, offset
 * Branch or Not Equal
 * Page 106 *)
let bne cpu ops =
  let ra = unsigned cpu.reg ops.(0) in
  let rb = unsigned cpu.reg ops.(1) in
  let off = signed imm ops.(2) in
  RTL.[
    when_ (lnot (ra = rb)) [
      cpu.jmp (cpu.cia + off)
    ]
  ]

(* NAL
 * No-op and Link, MIPS32 Release 6, deprecated
 * Page 358 *)
let nal cpu ops =
  let step = unsigned const byte 8 in
  RTL.[
    cpu.gpr 31 := cpu.cia + step;
  ]

let () =
  "BAL" >> bal;
  "BEQ" >> beq;
  "BEQL" >> beql;
  "BNE" >> bne;
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
  "JALR.HB" >> jalr;
  "JIALC" >> jialc;
  "JIC" >> jic;
  "JR" >> jr;
  "NAL" >> nal;
  "JALR64" >> jalr
