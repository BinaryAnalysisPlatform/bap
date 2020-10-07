open Bap_core_theory
open Base
open KB.Syntax

module Env  = Thumb_env.Env
module Flags = Thumb_flags.Flags
module Defs = Thumb_defs

exception Lift_Error = Thumb_defs.Lift_Error

module Mov(Core : Theory.Core) = struct
  open Core

  module Utils = Thumb_util.Utils(Core)
  module Flags = Flags(Core)
  module DSL = Thumb_dsl.Make(Core)

  open Utils

  let mover dest src = 
    DSL.[
      !$$+dest := !$+src
    ]

  let movesr dest src = 
    DSL.[
      !$$dest := !$src;
      Flags.set_nzf !$$dest
    ]

  let movei8 dest immsrc = 
    DSL.[
      !$$dest := !$immsrc;
      Flags.set_nzf !$$dest
    ]

  let movenot dest src =
    DSL.[
      !$$dest := not !$src;
      Flags.set_nzf !$$dest
    ]

  let mul dest src =
    DSL.[
      !$$dest := !$dest * !$src;
      Flags.set_nzf !$$dest
    ]

  let addi3 dest src immsrc = 
    DSL.[
      !$$dest := !$src + !$immsrc;
      Flags.set_add !$src !$immsrc !$$dest
    ]

  let subi3 dest src immsrc =
    DSL.[
      !$$dest := !$src - !$immsrc;
      Flags.set_sub !$src !$immsrc !$$dest
    ]

  (* a temp value is introduced here *)
  let addi8 dest immsrc =
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$dest;
        !$$dest := !$dest + !$immsrc;
        Flags.set_add (var tmp) !$immsrc !$$dest
      ]
    ]

  let subi8 dest immsrc = 
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$dest;
        !$$dest := !$dest - !$immsrc;
        Flags.set_sub (var tmp) !$immsrc !$$dest
      ]
    ]

  let addrr d s1 s2 = 
    DSL.[
      !$$d := !$s1 + !$s2;
      Flags.set_add !$s1 !$s2 !$$d
    ]

  let subrr d s1 s2 = 
    DSL.[
      !$$d := !$s1 - !$s2;
      Flags.set_sub !$s1 !$s2 !$$d
    ]

  let addhirr d s = 
    DSL.[
      !$$+d := !$+d + !$+s;
    ]

  (* Rd = (PC and 0xfffffffc) + (imm << 2) *)
  let adr dest immsrc addr = 
    DSL.[
      !$$dest := addr land (imm 0xFFFFFFFC) + !$immsrc << !!2
    ]

  let addrspi dest immsrc = 
    DSL.[
      !$$dest := (var Env.sp) + !$immsrc << !!2
    ]

  let addspi immsrc = 
    DSL.[
      Env.sp := (var Env.sp) + !$immsrc << !!2
    ]

  let subspi immsrc = 
    DSL.[
      Env.sp := (var Env.sp) - !$immsrc << !!2
    ]

  let adc d s = 
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$d;
        !$$d := !$d + !$s + bool_as_bitv (var Env.cf);
        Flags.set_adc (var tmp) !$s !$$d
      ]
    ]

  let sbc d s =
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$d;
        !$$d := !$s - !$d - bool_as_bitv (var Env.cf |> inv);
        Flags.set_sbc (var tmp) !$s !$$s
      ]
    ]

  let andrr dest src = 
    DSL.[
      !$$dest := !$dest land !$src;
      Flags.set_nzf !$$dest
    ]

  open Bap.Std

  let asri dest src srcimm =
    let shift_amt = Defs.assert_imm srcimm in
    DSL.[
      when_else_ (Word.is_zero shift_amt) [
        Env.cf := msb !$src;
        if_else_ (msb !$src) [
          !$$dest := !!0xffffffff
        ] (*else*) [
          !$$dest := !!0
        ]
      ] (*else*) [
        Env.cf := nth_bit (!$srcimm - !!1) !$src;
        !$$dest := !$src asr !$srcimm
      ];
      Flags.set_nzf !$$dest
    ]

  (* ASRr has a rather complex logic, see A7.1.12 *)
  let asrr dest src = 
    let seg = DSL.(extend_to Env.byte !$src |> extend) in
    DSL.[
      if_else_ (seg <+ !!32) [
        if_ (seg <> !!0) [
          Env.cf := nth_bit (seg - !!1) !$dest;
          !$$dest := !$dest asr seg
        ]
      ] (*else*) [
        Env.cf := msb !$dest;
        if_else_ (msb !$dest) [
          !$$dest := !!0xffffffff
        ] (*else*) [
          !$$dest := !!0
        ]
      ];
      Flags.set_nzf !$$dest
    ]

  let bic dest src = 
    DSL.[
      !$$dest := !$dest land lnot !$src;
      Flags.set_nzf !$$dest
    ]

  let cmnz dest src = 
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$dest + !$src;
        Flags.set_add !$dest !$src tmp
      ]
    ]

  let cmpi8 dest immsrc = 
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$dest - !$immsrc;
        Flags.set_sub !$dest !$immsrc tmp
      ]
    ]

  let cmpr dest src = 
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$dest - !$src;
        Flags.set_sub !$dest !$src tmp
      ]
    ]

  let cmphir dest src = 
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := dest - src;
        Flags.set_sub dest src tmp
      ]
    ]

  let eor dest src = 
    DSL.[
      !$$dest := !$dest lxor !$src;
      Flags.set_nzf !$$dest
    ]

  let lsli dest src immsrc = 
    let shift_amt = Defs.assert_imm immsrc in
    DSL.[
      when_else_ (Word.is_zero shift_amt) [
        !$$dest := !$src
      ] (*else*) [
        Env.cf := nth_bit (!!32 - !$immsrc) !$src;
        !$$dest := !$src << !$immsrc
      ];
      Flags.set_nzf !$$dest
    ]

  (* LSLr has a rather complex logic, see A7.1.39 *)
  let lslr dest src = 
    let seg = DSL.(extend_to Env.byte !$src |> extend) in
    DSL.[
      if_else_ (seg <+ !!32) [
        if_ (seg <> !!0) [
          Env.cf := nth_bit (!!32 - seg) !$dest;
          !$$dest := !$dest << seg
        ]
      ] (*else*) [
        if_else_ (seg = !!32) [
          Env.cf := lsb !$dest
        ] (*else*) [
          Env.cf := b0
        ];
        !$$dest := !!0
      ];
      Flags.set_nzf !$$dest
    ]

  let lsri dest src immsrc = 
    let shift_amt = Defs.assert_imm immsrc in
    DSL.[
      when_else_ (Word.is_zero shift_amt) [
        Env.cf := msb !$src;
        !$$dest := !!0
      ] (*else*) [
        Env.cf := nth_bit (!$immsrc - !!1) !$src;
        !$$dest := !$src >> !$immsrc
      ];
      Flags.set_nzf !$$dest
    ]

  let lsrr dest src = 
    let seg = DSL.(extend_to Env.byte !$src |> extend) in
    DSL.[
      if_else_ (seg <+ !!32) [
        if_ (seg <> !!0) [
          Env.cf := nth_bit (seg - !!1) !$dest;
          !$$dest := !$dest >> seg
        ]
      ] (*else*) [
        if_else_ (seg = !!32) [
          Env.cf := msb !$dest
        ] (*else*) [
          Env.cf := b0
        ];
        !$$dest := !!0
      ];
      Flags.set_nzf !$$dest
    ]

  let orr dest src = 
    DSL.[
      !$$dest := !$dest lor !$src;
      Flags.set_nzf !$$dest
    ]

  (* This is actually code named `neg Rd Rm`, but llvm encodes it as `rsb Rd Rm #0` *)
  let rsb dest src immsrc = 
    DSL.[
      !$$dest := !$immsrc - !$src;
      Flags.set_sub !$immsrc !$src !$$dest
    ]

  let rev dest src =
    DSL.[
      !$$dest := concat Env.value [
          extract Env.byte !!7 !!0 !$src;
          extract Env.byte !!15 !!8 !$src;
          extract Env.byte !!23 !!16 !$src;
          extract Env.byte !!31 !!24 !$src;
        ]
    ]

  let rev_halfword hf =
    let i8 = bitv_of 8 in
    logor (lshift hf i8) (rshift hf i8)

  let rev16 dest src = 
    DSL.[
      !$$dest := concat Env.value [
          extract Env.byte !!23 !!16 !$src;
          extract Env.byte !!31 !!24 !$src;
          extract Env.byte !!7 !!0 !$src;
          extract Env.byte !!15 !!8 !$src;
        ]
    ]

  let revsh dest src = 
    DSL.[
      if_else_ (msb (extend_to Env.byte !$src)) [
        !$$dest := concat Env.value [
            extend_to Env.byte !!0xff;
            extend_to Env.byte !!0xff;
            extract Env.byte !!7 !!0 !$src;
            extract Env.byte !!15 !!8 !$src;
          ]
      ] (*else*) [
        !$$dest := concat Env.value [
            extend_to Env.byte !!0x00;
            extend_to Env.byte !!0x00;
            extract Env.byte !!7 !!0 !$src;
            extract Env.byte !!15 !!8 !$src;
          ]
      ]
    ]

  let ror dest src = 
    let seg4 = DSL.(extend_to Env.half_byte !$src |> extend) in
    DSL.[
      if_else_ (seg4 = !!0) [
        Env.cf := msb !$dest
      ] (*else*) [
        Env.cf := nth_bit (seg4 - !!1) !$dest;
        !$$dest := !$dest >> seg4 lor !$dest << (!!32 - seg4) 
      ];
      Flags.set_nzf !$$dest
    ]

  let tst dest src = 
    DSL.[
      local_var >>= fun tmp -> !%[
        tmp := !$dest land !$src;
        Flags.set_nzf tmp
      ]
    ]

end