(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved.*)
(** Utility functions for ASTs.  It's useful to have these in a
    separate file so it can use functions from Typecheck and elsewhere. *)

module Bil = X86_legacy_bil

open Bil.Ast
open Bil.Type
open Core_kernel

module Arithmetic = X86_legacy_bil_arithmetic
module Big_int_convenience = X86_legacy_bil_big_int_convenience
module Typecheck = X86_legacy_bil_typecheck

open Big_int_convenience

(** Create a single target cjmp. Uses a hopefully-unique label for the other. *)
let cjmp c t =
  CJmp(c, t, [])

(* exp helpers *)
let unknown t s =
  Unknown(s, t)

let binop op a b = match op,a,b with
  | _, Int(a, at), Int(b, bt) ->
    assert (Poly.(=) at bt);
    let (i,t) = Arithmetic.binop op (a,at) (b,bt) in
    Int(i,t)
  | (LSHIFT|RSHIFT|ARSHIFT), _, Int(z, _) when bi_is_zero z -> a
  | _ -> BinOp(op, a, b)

let unop (op : Type.unop_type) a =
  (* TODO: avoid constant folding on floats until we get a proper
     FP-evaluation module *)
  match op with
  | FP _ -> UnOp(op, a)
  | _ -> begin match a with
      | Int(a, at) ->
        let (i,t) = Arithmetic.unop op (a,at) in
        Int(i,t)
      | _ -> UnOp(op, a)
    end

let concat a b = match a,b with
  | Int(a, at), Int(b, bt) ->
    let (i,t) = Arithmetic.concat (a,at) (b,bt) in
    Int(i,t)
  | _ -> Concat(a, b)

let extract h l e =
  let h = Big_int_Z.big_int_of_int h in
  let l = Big_int_Z.big_int_of_int l in
  match e with
  | Int(i, t) ->
    let (i,t) = Arithmetic.extract h l (i,t) in
    Int(i,t)
  | _ -> Extract(h, l, e)

(* More convenience functions for building common expressions. *)
let exp_and e1 e2 = binop AND e1 e2
let exp_or e1 e2 = binop OR e1 e2
let exp_eq e1 e2 = binop EQ e1 e2
let exp_not e = unop NOT e
let exp_implies e1 e2 = exp_or (exp_not e1) e2

let (exp_shl, exp_shr) =
  let s dir e1 = function
    | Int(i,_) when bi_is_zero i -> e1
    | e2 -> BinOp(dir, e1, e2)
  in
  (s LSHIFT, s RSHIFT)

let ( +* ) a b   = binop PLUS a b
let ( -* ) a b   = binop MINUS a b
let ( ** ) a b   = binop TIMES a b
let ( <<* ) a b  = binop LSHIFT a b
let ( >>* ) a b  = binop RSHIFT a b
let ( >>>* ) a b = binop ARSHIFT a b
let ( &* ) a b   = binop AND a b
let ( |* ) a b   = binop OR a b
let ( ^* ) a b   = binop XOR a b
let ( ==* ) a b  = binop EQ a b
let ( <>* ) a b  = binop NEQ a b
let ( <* ) a b   = binop LT a b
let ( >* ) a b   = binop LT b a
let (<=* ) a b   = binop LE a b
let (>=* ) a b   = binop LE b a
(** bitwise equality *)
let ( =* ) a b   = binop XOR a (unop NOT b)

let ( ++* ) a b   = concat a b
let ( %* ) a b = binop MOD a b
let ( $%* ) a b = binop SMOD a b
let ( /* ) a b = binop DIVIDE a b
let ( $/* ) a b = binop SDIVIDE a b

(* _maybe_float is ghetto, but useful for now. *)
let rec _maybe_float = function
  | Load (_,_,_,Float _)
  | BinOp (FP _,_,_)
  | UnOp (FP _,_)
  | Var (Var.V(_,_,Float _))
  | Int (_,Float _)
  | Unknown (_, Float _) -> true
  | Let (_, _, x)
  | Ite (_, x, _) -> _maybe_float x
  | _ -> false

let _fbinop op rm a b =
  (*if _maybe_float a && _maybe_float b *)
  binop (FP(op, rm)) a b
(*else Logs.failwith "Type error: fbinop on non-float." *)

let fadd ?(rm=RNE) a b = _fbinop FADD rm a b
let fsub ?(rm=RNE) a b = _fbinop FSUB rm a b
let fmul ?(rm=RNE) a b = _fbinop FMUL rm a b
let fdiv ?(rm=RNE) a b = _fbinop FDIV rm a b
let frem ?(rm=RNE) a b = _fbinop FREM rm a b
let fmin ?(rm=RNE) a b = _fbinop FMIN rm a b
let fmax ?(rm=RNE) a b = _fbinop FMAX rm a b
let fle ?(rm=RNE) a b = _fbinop FLE rm a b
let flt ?(rm=RNE) a b = _fbinop FLT rm a b
let feq ?(rm=RNE) a b = _fbinop FEQ rm a b

let _funop op rm a =
  (* if _maybe_float a *)
  (* then *) unop (FP(op, rm)) a
(*  else Logs.failwith "Type error: funop on non-float." *)

let fabs ?(rm=RNE) a = _funop FABS rm a
let fneg ?(rm=RNE) a = _funop FNEG rm a
let fsqrt ?(rm=RNE) a = _funop FSQRT rm a
let fround ?(rm=RNE) a = _funop FROUND rm a
let fisnorm ?(rm=RNE) a = _funop FISNORM rm a
let fissub ?(rm=RNE) a = _funop FISSUB rm a
let fiszero ?(rm=RNE) a = _funop FISZERO rm a
let fisinf ?(rm=RNE) a = _funop FISINF rm a
let fisnan ?(rm=RNE) a = _funop FISNAN rm a
let fisneg ?(rm=RNE) a = _funop FISNEG rm a
let fispos ?(rm=RNE) a = _funop FISPOS rm a

let ftoubv ?(rm=RNE) ~bv_size a = unop (FP(FFTOUBV bv_size, rm)) a
let ftosbv ?(rm=RNE) ~bv_size a = unop (FP(FFTOSBV bv_size, rm)) a
let ftoieeebv ?(rm=RNE) ~bv_size a = unop (FP(FFTOIEEEBV bv_size, rm)) a
let bvtouf ?(rm=RNE) ~float_size a = unop (FP(FBVTOUF float_size, rm)) a
let bvtosf ?(rm=RNE) ~float_size a = unop (FP(FBVTOSF float_size, rm)) a
let ftof ?(rm=RNE) ~float_size a = unop (FP(FFTOF float_size, rm)) a
let ieeebvtof ?(rm=RNE) ~float_size a = unop (FP(FIEEEBVTOF float_size, rm)) a
let fnan ?(rm=RNE) ~float_size = unop (FP(FNAN float_size, rm)) (exp_false)

let cast ct tnew = function
  | Int(i,t) -> let (i',t') = Arithmetic.cast ct (i,t) tnew in
    Int(i',t')
  | e when Poly.(=) tnew @@ Typecheck.infer_ast e -> e
  | e -> Cast(ct, tnew, e)

let cast_low = cast CAST_LOW
let cast_high = cast CAST_HIGH
let cast_signed = cast CAST_SIGNED
let rec cast_unsigned tnew = function
  | Cast(CAST_UNSIGNED, Reg t', e) when Arithmetic.bits_of_width tnew >= t' ->
    (* Recurse, since we might be able to simplify e further now *)
    cast_unsigned tnew e
  | e ->
    cast CAST_UNSIGNED tnew e

let exp_int i bits = Int(i, Reg bits)

(** Converts a signed integer of the given bit width to an unsigned big int. *)
let int_to_big_uint ~width ~value =
  let bi = biconst value in
  if value >= 0 then bi else
    let offset = Arithmetic.power_of_two width in
    bi +% offset

let it i t =
  match t with
  | Reg n ->
    Int(int_to_big_uint ~width:n ~value:i, t)
  | _ -> failwith "Unexpected type in Ast_convenience.it"

let exp_ite ?t b e1 e2 =
  (* type inference shouldn't be needed when t is specified, but we're paranoid *)
  let tb = Typecheck.infer_ast b in
  let t1 = Typecheck.infer_ast e1 in
  let t2 = Typecheck.infer_ast e2 in
  assert (Poly.(=) t1 t2);
  assert (Poly.(=) tb @@ Reg 1);

  (match t with
   | None -> ()
   | Some t -> assert (Poly.(=) t t1));

  if Poly.equal exp_true b then e1
  else if Poly.equal exp_false b then e2
  else Ite(b, e1, e2)

let min_symbolic ~signed e1 e2 =
  let bop = if signed then SLT else LT in
  exp_ite (binop bop e1 e2) e1 e2

let max_symbolic ~signed e1 e2 =
  let bop = if signed then SLT else LT in
  exp_ite (unop NOT (binop bop e1 e2)) e1 e2

(* Extract the nth least significant element of type t from e,
   starting with zero. n is a non-negative integer. *)
let extract_element t e n =
  let nbits = Typecheck.bits_of_width t in
  extract (n*nbits+(nbits-1)) (n*nbits) e

(* Extract the nth least significant byte from e, starting with
   zero. n is a non-negative integer *)
let extract_byte e n = extract_element reg_8 e n
