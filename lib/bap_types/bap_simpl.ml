open Core_kernel.Std

open Bap_common
open Bap_bil
open Binop
open Unop
open Exp
open Stmt

module Apply = Bap_apply
module Eff = Bap_eff
module Word = Bitvector

let zero width = Int (Word.zero width)
let ones width = Int (Word.ones width)
let nothing _ = false

let is0 = Word.is_zero and is1 = Word.is_one
let ism1 x = Word.is_zero (Word.lnot x)


(* requires: let-free, simplifications(
      constant-folding,
      neutral-element-elimination,
      zero-element-propagation)  *)
let removable ignore x =
  Eff.is_subset (Eff.compute x) (Eff.of_list ignore)

let is_associative = function
  | PLUS | TIMES | AND | OR | XOR -> true
  | _ -> false

let exp ?(ignore=[]) =
  let removable = removable ignore in
  let rec exp = function
    | Load (m,a,e,s) -> Load (exp m, exp a, e, s)
    | Store (m,a,v,e,s) -> Store (exp m, exp a, exp v, e, s)
    | BinOp (op,x,y) -> binop op x y
    | UnOp (op,x) -> unop op x
    | Var _ | Int _  | Unknown (_,_) as const -> const
    | Cast (t,s,x) -> cast t s x
    | Let (v,x,y) -> Let (v, exp x, exp y)
    | Ite (x,y,z) -> Ite (exp x, exp y, exp z)
    | Extract (h,l,x) -> extract h l x
    | Concat (x,y) -> concat x y
  and concat x y = match exp x, exp y with
    | Int x, Int y -> Int (Word.concat x y)
    | x,y -> Concat (x,y)
  and cast t s x = match exp x with
    | Int w -> Int (Apply.cast t s w)
    | _ -> Cast (t,s,x)
  and extract hi lo x = match exp x with
    | Int w -> Int (Bitvector.extract_exn ~hi ~lo w)
    | x -> Extract (hi,lo,x)
  and unop op x = match exp x with
    | UnOp(op,Int x) -> Int (Apply.unop op x)
    | UnOp(op',x) when op = op' -> exp x
    | x -> UnOp(op, x)
  and binop op x y =
    let width = match Bap_types_infer.infer_exn x with
      | Type.Imm s -> s
      | Type.Mem _ -> failwith "binop" in
    let keep op x y = BinOp(op,x,y) in
    let int f = function Int x -> f x | _ -> false in
    let is0 = int is0 and is1 = int is1 and ism1 = int ism1 in
    let op_eq x y = compare_binop x y = 0 in
    let (=) x y = compare_exp x y = 0 && removable x in
    match op, exp x, exp y with
    | op, Int x, Int y -> Int (Apply.binop op x y)
    | PLUS,x,y  when is0 x -> y
    | PLUS,x,y  when is0 y -> x
    | MINUS,x,y when is0 x -> UnOp(NEG,y)
    | MINUS,x,y when is0 y -> x
    | MINUS,x,y when x = y -> zero width
    | TIMES,x,y when is0 x && removable y -> x
    | TIMES,x,y when is0 y && removable x -> y
    | TIMES,x,y when is1 x -> y
    | TIMES,x,y when is1 y -> x
    | (DIVIDE|SDIVIDE),x,y when is1 y -> x
    | (MOD|SMOD),_,y when is1 y -> zero width
    | (LSHIFT|RSHIFT|ARSHIFT),x,y when is0 y -> x
    | (LSHIFT|RSHIFT|ARSHIFT),x,_ when is0 x -> x
    | (LSHIFT|RSHIFT|ARSHIFT),x,_ when ism1 x -> x
    | AND,x,y when is0 x && removable y -> x
    | AND,x,y when is0 y && removable x -> y
    | AND,x,y when ism1 x -> y
    | AND,x,y when ism1 y -> x
    | AND,x,y when x = y -> x
    | OR,x,y  when is0 x -> y
    | OR,x,y  when is0 y -> x
    | OR,x,y  when ism1 x && removable y -> x
    | OR,x,y  when ism1 y && removable x -> y
    | OR,x,y  when x = y -> x
    | XOR,x,y when x = y -> zero width
    | XOR,x,y when is0 x -> y
    | XOR,x,y when is0 y -> x
    | EQ,x,y  when x = y -> Int Word.b1
    | NEQ,x,y when x = y -> Int Word.b0
    | (LT|SLT), x, y when x = y -> Int Word.b0
    | op,BinOp(op',x, Int p),Int q
      when op_eq op op' && is_associative op ->
      BinOp (op,x,Int (Apply.binop op p q))
    | op,x,y -> keep op x y in
  exp

let bil ?ignore =
  let exp x = exp ?ignore x in
  let rec stmt = function
    | Move (v,x) -> [Move (v, exp x)]
    | Jmp x -> [Jmp (exp x)]
    | While (c,ss) -> while_ c ss
    | If (c,ts,fs) -> if_ c ts fs
    | s -> [s]
  and if_ c ts fs = match exp c with
    | Int x ->
      if Word.is_zero x then bil fs else bil ts
    | c -> [If (exp c, bil ts, bil fs)]
  and while_  c ss = match exp c with
    | Int x when Word.is_zero x -> []
    | c -> [While (c, bil ss)]
  and bil = List.concat_map ~f:stmt in
  bil

let stmt ?ignore x = bil ?ignore [x]
