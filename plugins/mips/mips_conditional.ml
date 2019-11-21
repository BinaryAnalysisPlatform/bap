open Mips.Std

(* SLT v0, v1, v0
 * Set on Less Than, MIPS32
 * Page 442 *)
let slt cpu ops =
  let rd = signed cpu.reg ops.(0) in
  let rs = signed cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    if_ (rs <$ rt) [
      rd := one;
    ] [
      rd := zero;
    ]
  ]

(* SLTI rt, rs, imm
 * Set on Less Than Immediate, MIPS32
 * Page 443 *)
let slti cpu ops =
  let rt = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = unsigned imm ops.(2) in
  RTL.[
    if_ (rs < im) [
      rt := one;
    ] [
      rt := zero;
    ]
  ]

(* SLTU v1, v0, s1
 * Set on Less Than Unsigned, MIPS32
 * Page 445 *)
let sltu cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    if_ (rs < rt) [
      rd := one;
    ] [
      rd := zero;
    ]
  ]

(* SLTiu v0, v0, 7
 * Set on Less Than Immediate Unsigned, MIPS32
 * Page 444 *)
let sltiu cpu ops =
  let rt = signed cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let im = signed imm ops.(2) in
  RTL.[
    if_ (rs < im) [
      rt := one;
    ] [
      rt := zero;
    ]
  ]

(* SELEQZ rd, rs, rt
 * Select integer GPR value or zero, MIPS32 Release 6
 * Page 431 *)
let seleqz cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    if_ (rt = zero) [
      rd := rs;
    ] [
      rd := zero;
    ]
  ]

(* SELNEZ rd, rs, rt
 * Select integer GPR value or zero, MIPS32 Release 6
 * Page 432 *)
let selnez cpu ops =
  let rd = unsigned cpu.reg ops.(0) in
  let rs = unsigned cpu.reg ops.(1) in
  let rt = unsigned cpu.reg ops.(2) in
  RTL.[
    if_ (rt <> zero) [
      rd := rs;
    ] [
      rd := zero;
    ]
  ]

let () =
  Bap_main.Extension.declare @@ fun _ctxt ->
  "SLT"  >> slt;
  "SLTi" >> slti;
  "SLTu"  >> sltu;
  "SLTiu" >> sltiu;
  "SELEQZ" >> seleqz;
  "SELNEQ" >> selnez;
  Ok ();
