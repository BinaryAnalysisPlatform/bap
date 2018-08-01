open Core_kernel
open Bap_common
open Bap_bil
open Binop
open Unop
open Exp
open Stmt
open Cast

module Type_error = Bap_type_error
module Word = Bitvector
module Var = Bap_var
module Size = Bap_size

(** [infer x] infers the type of the expression [x].  Either returns
    the inferred type, or terminates abnormally on the first type
    error with the `Type_error.E` exception.  *)
let rec infer = function
  | Var v -> Var.typ v
  | Int x -> Type.Imm (Word.bitwidth x)
  | Unknown (_,t) -> t
  | Load (m,a,_,s) -> load m a s
  | Cast (c,s,x) -> cast c s x
  | Store (m,a,x,_,t) -> store m a x t
  | BinOp (op,x,y) -> binop op x y
  | UnOp (_,x) -> unop x
  | Let (v,x,y) -> let_ v x y
  | Ite (c,x,y) -> ite c x y
  | Extract (hi,lo,x) -> extract hi lo x
  | Concat (x,y) -> concat x y
and unify x y =
  let t1 = infer x and t2 = infer y in
  if t1 = t2 then t1
  else Type_error.expect t1 ~got:t2
and let_ v x y =
  let t = Var.typ v and u = infer x in
  if t = u then infer y
  else Type_error.expect t ~got:u
and ite c x y = match infer c with
  | Type.Mem _ -> Type_error.expect_imm ()
  | Type.Imm 1 -> unify x y
  | t -> Type_error.expect (Type.Imm 1) ~got:t
and unop x = match infer x with
  | Type.Mem _ -> Type_error.expect_imm ()
  | t -> t
and binop op x y = match op with
  | LSHIFT|RSHIFT|ARSHIFT -> shift x y
  | _ -> match unify x y with
    | Type.Mem _ -> Type_error.expect_imm ()
    | Type.Imm _ as t -> match op with
      | LT|LE|EQ|NEQ|SLT|SLE -> Type.Imm 1
      | _ -> t
and shift x y = match infer x, infer y with
  | Type.Mem _,_ | _,Type.Mem _ -> Type_error.expect_imm ()
  | t, Type.Imm _ -> t
and load m a r = match infer m, infer a with
  | Type.Imm _,_ -> Type_error.expect_mem ()
  | _,Type.Mem _ -> Type_error.expect_imm ()
  | Type.Mem (s,_),Type.Imm s' ->
    let s = Size.in_bits s in
    if s = s' then Type.Imm (Size.in_bits r)
    else Type_error.expect (Type.Imm s) ~got:(Type.Imm s')
and store m a x _ =
  match infer m, infer a, infer x with
  | Type.Imm _,_,_ -> Type_error.expect_mem ()
  | Type.Mem (s,_) as t, Type.Imm s', Type.Imm u ->
    let s = Size.in_bits s in
    if s <> s'
    then Type_error.expect (Type.Imm s) ~got:(Type.Imm s')
    else if is_error (Size.of_int u)
    then Type_error.wrong_cast ()
    else t
  | _ -> Type_error.expect_imm ()
and cast c s x =
  let t = Type.Imm s in
  match c,infer x with
  | _,Type.Mem _ -> Type_error.expect_imm ()
  | (UNSIGNED|SIGNED),_ -> t
  | (HIGH|LOW), Type.Imm s' ->
    if s' >= s then t else Type_error.wrong_cast ()
and extract hi lo x = match infer x with
  | Type.Mem _ -> Type_error.expect_imm ()
  | Type.Imm _ ->
    (* we don't really need a type of x, as the extract operation
       can both narrow and widen. Though it is a question whether it is
       correct or not, especially wrt to the operational semantics, the
       real life fact is that our lifters are (ab)using extract
       instruction in both directions.  *)
    if hi >= lo then Type.Imm (hi - lo + 1)
    else Type_error.wrong_cast ()
and concat x y = match infer x, infer y with
  | Type.Imm s, Type.Imm t -> Type.Imm (s+t)
  | _ -> Type_error.expect_mem ()

let infer_exn = infer
let infer x =
  try Ok (infer x) with
  | Type_error.T err -> Error err

let (&&&) = Option.first_some


(** [check xs] verifies that [xs] is well-typed.  *)
let rec check bil =
  List.find_map bil ~f:(function
      | Move (v,x) -> move v x
      | Jmp d -> jmp d
      | While (c,xs) -> cond c &&& check xs
      | If (c,xs,ys) -> cond c &&& check xs &&& check ys
      | Special _ | CpuExn _ -> None)
and move v x =
  let t = Var.typ v in
  match infer x with
  | Ok u when t = u -> None
  | Ok u -> Some (Type_error.bad_type ~exp:t ~got:u)
  | Error err -> Some err
and jmp x = match infer x with
  | Ok (Imm s) when Result.is_ok (Size.addr_of_int s) -> None
  | Ok (Mem _) -> Some Type_error.bad_imm
  | Ok (Imm _) -> Some Type_error.bad_cast
  | Error err -> Some err
and cond x = match infer x with
  | Ok (Imm 1) -> None
  | Ok got -> Some (Type_error.bad_type ~exp:(Type.Imm 1) ~got)
  | Error err -> Some err

let check xs = match check xs with
  | None -> Ok ()
  | Some err -> Error err
