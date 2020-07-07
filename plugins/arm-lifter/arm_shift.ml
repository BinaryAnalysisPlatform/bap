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

end