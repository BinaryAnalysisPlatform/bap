open Mips.Std

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
