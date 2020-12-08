(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(** Basic integer arithmetic on N-bit integers

    These are common operations which are needed for constant folding or
    evaluation.

    @author Ivan Jager
*)

module Bil = X86_legacy_bil

open Big_int_Z
open X86_legacy_bil_big_int_convenience
open Bil.Type



exception ArithmeticEx of string

let memoize ?(size = 128) f =
  let results = Hashtbl.create size in
  fun x ->
    try Hashtbl.find results x
    with Not_found ->
      let y = f x in
      Hashtbl.add results x y;
      y

let power_of_two = memoize (shift_left_big_int bi1)
let bitmask = memoize (fun i -> power_of_two i -% bi1)

let bits_of_width = function
  | Reg n -> n
  | Float float_size -> float_size.exp_bits + float_size.sig_bits
  | _ -> failwith "Expected register type"

(* drop high bits to type t *)
let to_big_int (i,t) =
  let bits = bits_of_width t in
  and_big_int i (bitmask bits)

(* sign extend to type t *)
let to_sbig_int (i,t) =
  let bits = bits_of_width t in
  let final = to_big_int (i, Reg(bits-1)) in
  (* mod always returns a positive number *)
  let sign = i >>% (bits-1) in
  if bi_is_zero sign then (* positive *) final else (* negative *) minus_big_int ((power_of_two (bits-1) -% final))

let t_div dividend divisor =
  Z.div dividend divisor

let t_mod dividend divisor =
  Z.rem dividend divisor

(* shifting by more than the number of bits or by negative values will
 * be the same as shifting by the max number of bits. *)
let toshift shiftedt v =
  let max = bits_of_width shiftedt
  and i = to_big_int v in
  assert (i >=% bi0);
  let max_bi = (big_int_of_int max) in
  if i <=% max_bi then
    int_of_big_int i
  else
    let error_str = Caml.Printf.sprintf "shifting %d-bit value by %s" max (string_of_big_int i) in
    let () = failwith error_str
    in max

(* "cast" an int64 to a value *)
let to_val t i =
  (to_big_int (i,t), t)

let exp_bool =
  let t = (unit_big_int, Reg(1))
  and f = (zero_big_int, Reg(1)) in
  (fun b -> if b then t else f)

(** [binop operand lhs lhst rhs rhst] *)
let binop op ((_,t) as v1) v2 =
  match op with
  | PLUS -> to_val t (add_big_int (to_big_int v1) (to_big_int v2))
  | MINUS -> to_val t (sub_big_int (to_big_int v1) (to_big_int v2))
  | TIMES -> to_val t (mult_big_int (to_big_int v1) (to_big_int v2))
  | AND -> to_val t (and_big_int (to_big_int v1) (to_big_int v2))
  | OR -> to_val t (or_big_int (to_big_int v1) (to_big_int v2))
  | XOR -> to_val t (xor_big_int (to_big_int v1) (to_big_int v2))
  | EQ -> exp_bool(eq_big_int (to_big_int v1) (to_big_int v2))
  | NEQ -> exp_bool(not (eq_big_int (to_big_int v1) (to_big_int v2)))
  | LSHIFT -> to_val t (shift_left_big_int (to_big_int v1) (toshift t v2))
  | RSHIFT -> to_val t (shift_right_big_int (to_big_int v1) (toshift t v2))
  | ARSHIFT -> to_val t (shift_right_big_int (to_sbig_int v1) (toshift t v2))
  | DIVIDE -> to_val t (div_big_int (to_big_int v1) (to_big_int v2))
  | SDIVIDE -> to_val t (t_div (to_sbig_int v1) (to_sbig_int v2))
  | MOD -> to_val t (mod_big_int (to_big_int v1) (to_big_int v2))
  | SMOD -> to_val t (t_mod (to_sbig_int v1) (to_sbig_int v2))
  | SLT -> exp_bool(lt_big_int (to_sbig_int v1) (to_sbig_int v2))
  | SLE -> exp_bool(le_big_int (to_sbig_int v1) (to_sbig_int v2))
  | LT -> exp_bool(lt_big_int (to_big_int v1) (to_big_int v2))
  | LE -> exp_bool(le_big_int (to_big_int v1) (to_big_int v2))
  | FP _ -> failwith "Concrete arithmetic on floating point is not allowed"


let unop op ((_,t) as v) =
  match op with
  | NEG -> to_val t (minus_big_int (to_big_int v))
  | NOT -> (* implemented as xor with -1 *)
    to_val t (xor_big_int (to_big_int (bim1,t)) (to_big_int v))

  | FP _ -> failwith "Concrete arithmetic on floating point is not allowed"

let cast ct ((_,t) as v) t2 =
  let bits1 = bits_of_width t
  and bits = bits_of_width t2 in
  (match ct with
   | CAST_UNSIGNED ->
     to_val t2 (to_big_int v)
   | CAST_SIGNED ->
     to_val t2 (to_sbig_int v)
   | CAST_HIGH ->
     to_val t2
       (shift_right_big_int (to_big_int v) (bits1-bits))
   | CAST_LOW ->
     to_val t2 (to_big_int v)
  )


let extract h l ((_,t) as v) =
  let n = (h -% l) +% bi1 in
  let nt = Reg(int_of_big_int n) in
  let s = binop RSHIFT v (l,t) in
  cast CAST_LOW s nt


let concat ((_,lt) as lv) ((_,rt) as rv) =
  let bitsl,bitsr =
    match lt, rt with
    | Reg(bitsl), Reg(bitsr) -> bitsl, bitsr
    | _ -> failwith "concat"
  in
  let nt = Reg(bitsl + bitsr) in
  let lv = cast CAST_UNSIGNED lv nt in
  let rv = cast CAST_UNSIGNED rv nt in
  let lv = binop LSHIFT lv (biconst bitsr, nt) in
  binop OR lv rv


let is_zero v =
  let v = to_big_int v in
  sign_big_int v = 0

let bytes_to_int64 e bs =
  match e with
  | `Little -> List.fold_right (fun b i ->
      let old = Int64.shift_left i 8 in
      Int64.add old b) bs 0L
  | `Big -> List.fold_left (fun i b ->
      let old = Int64.shift_left i 8 in
      Int64.add old b) 0L bs
