open Powerpc.Std

let update_link_register cpu ops =
  RTL.[cpu.lr := cpu.pc + unsigned const byte 4]

(** Branch Instructions, Branch
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    4b ff fe f0  b .+67108592
    4b ff fe f2  ba 67108592
    4b ff fe f1  bl .+67108592
    4b ff fe f3  bla 67108592 *)
let b cpu ops =
  let im = signed imm ops.(0) in
  let tm = signed var cpu.word_width in
  let sh = unsigned const byte 2 in
  RTL.[
    tm := last (im << sh) 26;
    cpu.jmp (cpu.pc + tm)
  ]

let ba cpu ops =
  let im = signed imm ops.(0) in
  let tm = signed var cpu.word_width in
  let sh = unsigned const byte 2 in
  RTL.[
    tm := last (im << sh) 26;
    cpu.jmp tm
  ]

let bl = update_link_register ^ b
let bla = update_link_register ^ ba

(** Branch Instructions, Branch Conditional
    Page 37 of IBM Power ISATM Version 3.0 B
    examples:
    42 9f 00 04  bc 20, 31, .+4
    42 9f 00 06  bca 20, 31, 4
    42 9f 00 05  bcl 20, 31, .+4
    42 9f 00 07  bcla 20, 31, 4   *)
let bc cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
  let bd = signed imm ops.(2) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  let tm = signed var cpu.word_width in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      tm := last (bd << sh) 16;
      cpu.jmp (cpu.pc + tm);
    ]
  ]

let bca cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
  let bd = signed imm ops.(2) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  let tm = signed var cpu.word_width in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      tm := last (bd << sh) 16;
      cpu.jmp tm;
    ]
  ]

let bcl = update_link_register ^ bc
let bcla = update_link_register ^ bca

(** bdz  target = bc 18,0, target *)
let bdz cpu ops =
  let bd = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let tm = signed var cpu.word_width in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (low cpu.word_width cpu.ctr = zero) [
      tm := bd << sh;
      cpu.jmp (cpu.pc + tm)
    ]
  ]

(** bdnz  target = bc 16,0, target *)
let bdnz cpu ops =
  let bd = unsigned imm ops.(0) in
  let sh = unsigned const byte 2 in
  let tm = signed var cpu.word_width in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (low cpu.word_width cpu.ctr <> zero) [
      tm := bd << sh;
      cpu.jmp (cpu.pc + tm)
    ]
  ]

(** Branch Instructions, Branch Conditional to Link Cpu.Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 00 20 	bclr	 20, 31
    4e 9f 00 21 	bclrl	 20, 31 *)
let bclr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  let tm = unsigned var doubleword in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      tm := first cpu.lr 62;
      cpu.jmp (tm << sh);
    ];
  ]

let bclrl cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
  let sh = unsigned const byte 2 in
  let ctr_ok = unsigned var bit in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  let tm = unsigned var doubleword in
  let step = unsigned const byte 4 in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    tm := first cpu.lr 62;
    cpu.lr := cpu.pc + step;
    when_ (ctr_ok land cond_ok) [
      cpu.jmp (tm << sh);
    ];
  ]

(** Branch Instructions extended mnemonic, branch to LR unconditionally.
    Page 792 of IBM Power ISATM Version 3.0 B
    examples:
    4e 80 00 20   blr
    4e 80 00 21   blrl *)
let blr cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.jmp (cpu.lr << sh)
  ]

let blrl cpu ops =
  let sh = unsigned const byte 2 in
  let tm = unsigned var doubleword in
  RTL.[
    tm := first cpu.ctr 62;
    cpu.lr := cpu.pc + unsigned const byte 4;
    cpu.jmp (tm << sh);
  ]

(** bdnzlr = bclr 16,0,0  *)
let bdnzlr cpu ops =
  let sh = unsigned const byte 2 in
  RTL.[
    cpu.ctr := cpu.ctr - one;
    when_ (cpu.ctr <> zero) [
      cpu.jmp (cpu.lr << sh);
    ];
  ]

(** Branch Instructions, Branch Conditional to Count Cpu.Register
    Page 38 of IBM Power ISATM Version 3.0 B
    examples:
    4d 5f 04 20    bcctr 10,31
    4d 5f 04 21    bcctrl 10,31 *)
let bcctr cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
  let cond_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  let tm = unsigned var doubleword in
  let sh = unsigned const doubleword 2 in
  RTL.[
    x := last bo 5;
    cond_ok := (nth bit x 0 = one) lor (bi lxor (lnot (nth bit x 1)));
    when_ (cond_ok) [
      tm := first cpu.ctr 62;
      cpu.jmp (tm << sh);
    ];
  ]

let bcctrl = update_link_register ^ bcctr

(** Branch Instructions extended mnemonic, branch to CTR unconditionally.
    Page 792 of IBM Power ISATM Version 3.0 B
    examples:
    4e 80 04 20   bctr
    4e 80 04 21   bctrl *)
let bctr cpu ops =
  let tm = unsigned var doubleword in
  let sh = unsigned const doubleword 2 in
  RTL.[
    tm := first cpu.ctr 62;
    cpu.jmp (tm << sh);
  ]

let bctrl = update_link_register ^ bctr

(** Branch Instructions, Branch Conditional to Target Cpu.Register
    Page 39 of IBM Power ISATM Version 3.0 B
    examples:
    4e 9f 04 60    bctar
    4e 9f 04 61    bctarl *)
let bctar cpu ops =
  let bo = unsigned imm ops.(0) in
  let bi = unsigned cpu.reg ops.(1) in
  let sh = unsigned const byte 2 in
  let cond_ok = unsigned var bit in
  let ctr_ok = unsigned var bit in
  let x = unsigned var (bitwidth 5) in
  RTL.[
    x := last bo 5;
    when_ (nth bit x 2 = zero) [
      cpu.ctr := cpu.ctr - one;
    ];
    ctr_ok := nth bit x 2 lor ((cpu.ctr <> zero) lxor (nth bit x 3));
    cond_ok := nth bit x 0 lor (bi lxor (lnot (nth bit x 1)));
    when_ (ctr_ok land cond_ok) [
      cpu.jmp (cpu.tar << sh);
    ]
  ]

let bctarl = update_link_register ^ bctar

let init () =
  "B"       >| b;
  "BA"      >| ba;
  "BL"      >| bl;
  "BLA"     >| bla;
  "gBC"     >| bc;
  "gBCA"    >| bca;
  "gBCL"    >| bcl;
  "gBCLA"   >| bcla;
  "BDZ"     >| bdz;
  "BDNZ"    >| bdnz;
  "BCC"     >| bc;
  "BCCL"    >| bcl;
  "BCCLA"   >| bcla;
  "gBCLR"   >| bclr;
  "gBCLRL"  >| bclrl;
  "gBCCTR"  >| bcctr;
  "gBCCTRL" >| bcctrl;
  "BDNZLR"  >| bdnzlr;
  "gBCTAR"  >| bctar;
  "gBCTARL" >| bctarl;
  "BLR"     >| blr;
  "BLRL"    >| blrl;
  "BCTR"    >| bctr;
  "BCTRL"   >| bctrl;
  "BCCLR"   >| bclr;
  "BCCLRL"  >| bclrl;
  "BCCCTR"  >| bcctr;
  "BCCCTRL" >| bcctrl;
