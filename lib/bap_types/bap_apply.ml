open Core_kernel.Std
open Bap_common
open Bap_bil
open Binop
open Unop

module Word = Bitvector

(* maps BIL expressions to Word operations *)

let is_shift = function
  | LSHIFT | RSHIFT | ARSHIFT -> true
  | _ -> false

let unop op u = match op with
  | NEG -> Word.neg u
  | NOT -> Word.lnot u

let binop op u v =
  let open Word in
  if Int.(bitwidth u <> bitwidth v) && not (is_shift op)
  then failwithf "binop type error - %s %s %s"
      (to_string u)
      (Bap_exp.Binop.string_of_binop op)
      (to_string v) ();
  match op with
  | PLUS -> u + v
  | MINUS -> u - v
  | TIMES -> u * v
  | DIVIDE -> u / v
  | SDIVIDE -> signed u / signed v
  | MOD -> u mod v
  | SMOD -> signed u mod signed v
  | LSHIFT -> u lsl v
  | RSHIFT -> u lsr v
  | ARSHIFT -> u asr v
  | AND -> u land v
  | OR -> u lor v
  | XOR -> u lxor v
  | EQ -> Bitvector.(of_bool (u = v))
  | NEQ -> Bitvector.(of_bool (u <> v))
  | LT -> Bitvector.(of_bool (u < v))
  | LE -> Bitvector.(of_bool (u <= v))
  | SLT -> Bitvector.(of_bool (signed u < signed v))
  | SLE  -> Bitvector.(of_bool (signed u <= signed v))

let cast ct sz u =
  let ext = Bitvector.extract_exn in
  match ct with
  | Cast.UNSIGNED -> ext ~hi:Int.(sz - 1) u
  | Cast.SIGNED   -> ext ~hi:Int.(sz - 1) (Bitvector.signed u)
  | Cast.HIGH     -> ext ~lo:Int.(Bitvector.bitwidth u - sz) u
  | Cast.LOW      -> ext ~hi:Int.(sz - 1) u
