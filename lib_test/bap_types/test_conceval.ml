open Core_kernel.Std
open OUnit2
open Bap_types.Std

open Conceval

let i0_8   = Word.of_int ~width:8 0
let i1_8   = Word.of_int ~width:8 1
let i3_8   = Word.of_int ~width:8 3
let i9_8   = Word.of_int ~width:8 9
let i101_8 = Word.of_int ~width:8 101

let test_word = Word.of_int32 0xDEADBEEFl
let zero = BV (Word.of_int32 0l)
let one  = BV (Word.of_int32 1l)
let two  = BV (Word.of_int32 2l)

let empty = Memory.empty


let printer = to_string

let option_printer = function
  | None -> "None"
  | Some v -> sprintf "Some (%s)" @@ to_string v

let suite =
  "Conceval" >:::
  [
    "Memory" >:::
    [
      "Load without write" >:: (fun ctxt ->
          let mem = empty in
          assert_equal ~ctxt ~printer:option_printer
            (Memory.load mem (BV i0_8) LittleEndian `r32)
            None;
        );

      "Basic write and load" >:: (fun ctxt ->
          let mem = Memory.store
              ~mem:empty
              ~idx:zero
              ~data:(BV (Addr.of_int32 0xDEADBEEFl)) LittleEndian `r32 in
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV test_word))
            (Memory.load ~mem ~idx:zero LittleEndian `r32);
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Bitvector.of_int32 0xEFBEADDEl)))
            (Memory.load ~mem ~idx:zero BigEndian `r32);
        );

      "Read low" >:: (fun ctxt ->
          let mem = Memory.store
              ~mem:empty
              ~idx:zero
              ~data:(BV test_word)
              LittleEndian `r32 in

          assert_equal ~ctxt ~printer:option_printer
            (Some (BV Word.(of_int 0xBEEF ~width:16)))
            (Memory.load ~mem ~idx:zero LittleEndian `r16);

          assert_equal ~ctxt ~printer:option_printer
            (Some (BV Word.(of_int 0xEFBE ~width:16)))
            (Memory.load ~mem ~idx:zero BigEndian `r16);
        );

      "Read high" >:: (fun ctxt ->
          let mem = Memory.store
              ~mem:empty ~idx:zero
              ~data:(BV (Word.of_int32 0xdeadbeefl))
              LittleEndian `r32 in
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xDEAD ~width:16)))
            (Memory.load ~mem ~idx:two LittleEndian `r16);
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xADDE ~width:16)))
            (Memory.load ~mem ~idx:two BigEndian `r16);
        );

      "Read middle" >:: (fun ctxt ->
          let mem = Memory.store
              ~mem:empty ~idx:zero
              ~data:(BV test_word) LittleEndian `r32 in
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xADBE ~width:16)))
            (Memory.load ~mem ~idx:one LittleEndian `r16);
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xBEAD ~width:16)))
            (Memory.load ~mem ~idx:one BigEndian `r16);
        );

      "Read low (bigendian)" >:: (fun ctxt ->
          let mem = Memory.store
              ~mem:empty ~idx:zero
              ~data:(BV (Word.of_int32 0xefbeaddel))
              BigEndian `r32 in
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xBEEF ~width:16)))
            (Memory.load ~mem ~idx:zero LittleEndian `r16);
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xEFBE ~width:16)))
            (Memory.load ~mem ~idx:zero BigEndian `r16);
        );

      "Read high (bigendian)" >:: (fun ctxt ->
          let mem = Memory.store
              ~mem:empty ~idx:zero
              ~data:(BV (Bitvector.of_int32 0xefbeaddel))
              BigEndian `r32 in
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xDEAD ~width:16)))
            (Memory.load ~mem ~idx:two LittleEndian `r16);
          assert_equal ~ctxt ~printer:option_printer
            (Memory.load ~mem ~idx:two BigEndian `r16)
            (Some (BV (Word.of_int 0xADDE ~width:16)));
        );

      "Read middle (bigendian)" >:: (fun ctxt ->
          let mem = Memory.store
              ~mem:empty ~idx:zero
              ~data:(BV (Word.of_int32 0xefbeaddel)) BigEndian `r32 in
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xADBE ~width:16)))
            (Memory.load ~mem ~idx:one LittleEndian `r16);
          assert_equal ~ctxt ~printer:option_printer
            (Some (BV (Word.of_int 0xBEAD ~width:16)))
            (Memory.load ~mem ~idx:one BigEndian `r16);
        );
    ];
    "eval_exp" >:::
    [
      "BinOp" >:: (fun ctxt ->
          let state = State.empty in
          assert_equal ~ctxt
            (eval_exp state Exp.(int i3_8 * int i3_8))
            (BV i9_8));
      "UnOp" >:: (fun ctxt ->
          let state = State.empty in
          assert_equal ~ctxt
            (eval_exp state Exp.(lnot (int i3_8)))
            (BV (Word.of_int ~width:8 (-4))));
    ];
    "eval_stmt" >:::
    [
      "Move" >:: (fun ctxt ->
          let state = State.empty in
          let var = Var.create "Garfield" reg8_t in
          let state, _ = eval_stmt state Stmt.(move var Exp.(int i3_8)) in
          assert_equal ~ctxt
            (State.peek state var)
            (Some (BV i3_8))
        );
      "While" >:: (fun ctxt ->
          let v = Var.create "Daisy" reg8_t in
          let state, _ =
            eval_stmt
              State.empty Stmt.(move v Exp.(int i0_8)) in
          let cond = Exp.(int i101_8 <> var v) in
          let body = Stmt.([
              move v Exp.(int i1_8 + var v);
            ]) in
          let state, _ = eval_stmt state Stmt.(While (cond, body)) in
          assert_equal
            (State.peek state v)
            (Some (BV (Word.of_int 101 ~width:8)))
        );
      "collatz" >:: (fun ctxt ->
          (* Starting with 17, we take 12 steps in the `3x+1` problem (OEIS). *)
          let v_steps = Var.create "nsteps" reg64_t in
          let v_n = Var.create ~tmp:true "n" reg64_t in
          let i17_64 = Word.of_int64 17L in
          let zero = Word.of_int64 0L  in
          let one  = Word.of_int64 1L  in
          let two  = Word.of_int64 2L  in
          let three  = Word.of_int64 3L  in
          let prog = [
            Stmt.(move v_n Exp.(int i17_64));
            Stmt.(move v_steps Exp.(int zero));
            Stmt.(While (Exp.(int one <> var v_n), [
                If (Exp.(int zero = var v_n mod int two),
                    [move v_n Exp.(var v_n / int two)],
                    [move v_n Exp.(var v_n * int three + int one)]);
                move v_steps Exp.(var v_steps + int one)
              ]))
          ] in
          (* Format.(fprintf std_formatter "@.@[program@ =@ %a@.@]" *)
          (*           Stmt.pp_stmts prog); *)
          let state, _ = eval_stmts State.empty prog in
          assert_equal ~ctxt (State.peek state v_n) None;
          assert_equal ~ctxt
            (State.peek state v_steps)
            (Some (BV (Bitvector.of_int 12 ~width:64)))
        );
    ];
  ]
