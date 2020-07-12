open Bap_core_theory
open Base
open KB.Syntax
open Bap.Std

module Env  = Arm_env.Env
module Defs = Arm_defs

module Shift(Core : Theory.Core) = struct
  open Core
  module DSL = Arm_dsl.Make(Core)

  let shift_of_word op = match Word.to_int op with
    | Ok 1 -> `ASR
    | Ok 2 -> `LSL
    | Ok 3 -> `LSR
    | Ok 4 -> `ROR
    | Ok 5 -> `RRX
    | _ -> raise (Defs.Lift_Error "unrecognized shifting value")

  let shift_decode op =
    let shift_type = Defs.assert_imm op in
    let width = Word.bitwidth shift_type in
    let mask = Word.of_int 7 ~width in
    let three = Word.of_int 3 ~width in
    (* lower three bits are type*)
    let r =
      let open Or_error in
      Word.Int_err.(!$shift_type land !$mask) >>| shift_of_word >>=
      fun shift_t ->
      (* other bits are immediate *)
      Word.Int_err.((!$shift_type land (lnot !$mask)) lsr !$three) >>=
      fun shift_amt ->
      return (shift_t, shift_amt) in r

  let shift_with_carry src shift_type shift =
    let bits_e = DSL.imm 32 in
    match shift_type with
    | `ASR ->
      let shifted = DSL.(src asr shift) in
      let carry = DSL.(nth_bit (shift - imm 1) src) in
      shifted, carry
    | `LSL ->
      let shifted = DSL.(src << shift) in
      let carry = DSL.(ite (shift <> imm 0)
                         (nth_bit (bits_e - shift) src)
                         (var Env.cf)) in
      shifted, carry
    | `LSR ->
      let shifted = DSL.(src >> shift) in
      let carry = DSL.(nth_bit (shift - imm 1) src) in
      shifted, carry
    | `ROR ->
      let ret1 = DSL.(src >> shift) in
      let ret2 = DSL.(src << (bits_e - shift)) in
      let shifted = DSL.(ret1 lor ret2) in
      let carry = DSL.(nth_bit (shift - imm 1) src) in
      shifted, carry
    | `RRX ->
      let ret1 = DSL.(src >> imm 1) in
      let carryin = DSL.(bool_as_bitv (var Env.cf) << (bits_e - imm 1)) in
      let shifted = DSL.(ret1 lor carryin) in
      let carry = DSL.(nth_bit (imm 1) src) in
      shifted, carry

  let shift_r src simm shift = 
    let shift_type = shift_of_word (Defs.assert_imm simm) in
    DSL.(shift_with_carry (assert_val src)
           shift_type (assert_val shift))

  let shift_i src simm =
    match shift_decode simm with
    | Ok(shift_type, shift_amt) ->
      DSL.(shift_with_carry (assert_val src) 
             shift_type (word_as_bitv shift_amt))
    | _ -> raise (Defs.Lift_Error "shift decoding failed")

  (* decodes a shifted operand for a memory operation
   * src - the operand to be shifted
   * shift - an int64,
   *            bits 11 through 0 represent the shift amount
   *            bits 12 represents whether the expression is added or subtracted
   *            bits 15 through 13 represent the shift type, valid shift types
   *              are number 1 through 5
   * typ - the type
   **)
  let mem_shift ~src shift =
    let shift = Defs.assert_imm shift in
    let width = 32 in
    let word = Word.of_int ~width in
    let wordm n = Ok (word n) in
    let shift_typ w =
      Word.Int_err.((!$w land wordm 0xE000) lsr wordm 13 >>|
                    shift_of_word) in
    (* Gets the shift amount from the immediate *)
    let shift_amt w = Word.Int_err.(!$w land wordm 0xFFF >>| DSL.word_as_bitv) in
    (* Converts the shift to a negative if the negative bit is set *)
    let to_neg w exp =
      match Word.Int_err.(wordm 0x1000 land !$w) with
      | Ok x when Word.equal x (word 0x1000) ->
        DSL.(word_as_bitv (Word.ones width) * exp)
      | _ -> exp in
    let r = Word.Int_err.(shift_typ shift >>= fun t -> shift_amt shift >>= fun amt ->
                          Or_error.return (t,amt)) in
    match r with
    | Error _ -> raise (Defs.Lift_Error "error during shift decoding")
    | Ok (t,amount) ->
      let exp, _ = shift_with_carry src t amount in
      to_neg shift exp

end