(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(* Type checking for BAP. *)
module Bil = X86_legacy_bil

open Big_int_Z
open Bil
open Bil.Ast
open Bil.Type

module Big_int_convenience = X86_legacy_bil_big_int_convenience
module Pp = X86_legacy_bil_pp

open Big_int_convenience

exception TypeError of string

let terror s = raise(TypeError s)

(* returns true if t1 equals t2 *)
let eq t1 t2 =
  t1 = t2

(* returns true if t1 is a multiple of t2 *)
let mult t1 t2 = match t1, t2 with
  | Reg x, Reg y -> x mod y = 0
  (* Can we do anything else for memory types? *)
  | x, y -> eq x y

let check checkf errmsg t1 t2 =
  if not (checkf t1 t2) then
    terror (Printf.sprintf errmsg (Pp.typ_to_string t1) (Pp.typ_to_string t2))

let check_eq t1 t2 f =
  check eq f t1 t2

let check_mult t1 t2 f =
  check mult f t1 t2

(* let check_eq t1 t2 f = *)
(*   if not (eq t1 t2) then *)
(*     terror (Printf.sprintf f (Pp.typ_to_string t1) (Pp.typ_to_string t2)) *)

let is_integer_type = function
  | Reg _ -> true
  | TMem _ | Array _ | Float _ -> false

let is_mem_type = function
  | TMem _ | Array _  -> true
  | Reg _ | Float _ -> false

let is_float_type = function
  | Float _ -> true
  | Reg _ | TMem _ | Array _ -> false

let index_type_of = function
  | TMem (it, _) | Array (it, _) -> it
  | Reg _ | Float _ -> invalid_arg "index_type_of"

let value_type_of = function
  | TMem (_, vt) -> vt
  | Array (_, vt) -> vt
  | Reg _ | Float _ -> invalid_arg "value_type_of"

let bits_of_width = function
  | Reg n -> n
  | _ -> invalid_arg "bits_of_width"

let bytes_of_width t =
  let b = bits_of_width t in
  if not ((b mod 8) = 0) then invalid_arg "bytes_of_width";
  b / 8

let rec infer_ast_internal check e =
  match e with
  | Var v ->
    (* FIXME: Check context *)
    Var.typ v
  | UnOp(FP ((FFTOUBV bv|FFTOSBV bv|FFTOIEEEBV bv), _), _) ->
    Reg bv
  | UnOp(FP ((FISNORM|FISSUB|FISZERO|FISINF|FISNAN|FISNEG|FISPOS), _), _) ->
    Reg 1
  | UnOp(FP ((FFTOF bv|FIEEEBVTOF bv|FBVTOSF bv|FBVTOUF bv|FNAN bv), _), _) ->
    Float bv
  | UnOp(_, e) ->
    if check then
      (let t = infer_ast_internal true e in
       check_reg t);
    infer_ast_internal false e;
  | BinOp(o,e1,e2) as e ->
    if check then (
      let t1 = infer_ast_internal true e1
      and t2 = infer_ast_internal true e2 in
      check_same t1 t2 ~e;
      match o with
      | EQ | NEQ | LT | LE | SLT | SLE | (FP((FEQ|FLT|FLE), _)) -> ()
      | _ -> check_reg t1);
    (match o with
     | EQ | NEQ | LT | LE | SLT | SLE | (FP((FEQ|FLT|FLE), _)) -> reg_1
     | _ -> infer_ast_internal false e1
    )
  | Ite(b,e1,e2) ->
    if check then
      (let t1 = infer_ast_internal true e1
       and t2 = infer_ast_internal true e2 in
       check_same t1 t2);
    infer_ast_internal false e1
  | Extract(h,l,e) ->
    let ns = int_of_big_int(h -% l +% bi1) in
    let nt = Reg ns in
    if check then (
      match infer_ast_internal true e with
      | Reg(oldn) ->
        if (ns <= 0) then terror("Extract must extract at least one bit");
        if l <% bi0 then terror("Lower bit index must be at least 0");
        if h >% (big_int_of_int oldn) -% bi1 then terror("Upper bit index must be at most one less than the size of the original register")
      | _ -> terror ("Extract expects Reg type")
    );
    nt
  | Concat(le, re) ->
    let lt, rt = infer_ast_internal check le, infer_ast_internal check re in
    let nt = match lt, rt with
      | Reg(lb), Reg(rb) ->
        Reg(lb+rb)
      | _ -> terror "Concat expects Reg type"
    in
    nt
  | Lab s ->
    (* FIXME: no type for labels yet *)
    reg_64
  | Int(i, t) ->
    if i < bi0 then terror("Int must be >= 0.");
    t
  | Unknown(_,t) ->
    t
  | Cast(ct,t,e) ->
    let te = infer_ast_internal check e in
    if check then (
      check_reg t;
      check_reg te;
      let bitse = bits_of_width te in
      let bitst = bits_of_width t in
      match ct with
      | CAST_UNSIGNED
      | CAST_SIGNED ->
        if bitst < bitse then terror (Printf.sprintf "Cast type %s is a widening case, but it was used to narrow %s to %s" (Pp.ct_to_string ct) (Pp.typ_to_string te) (Pp.typ_to_string t))
      | CAST_HIGH
      | CAST_LOW ->
        if bitst > bitse then terror (Printf.sprintf "Cast type %s is a narrowing case, but it was used to widen %s to %s" (Pp.ct_to_string ct) (Pp.typ_to_string te) (Pp.typ_to_string t))
    );
    t
  | Let(v,e1,e2) ->
    (* XXX: Need a type context to check this correctly *)
    if check then ignore(infer_ast_internal true e1);
    infer_ast_internal check e2
  | Load(arr,idx,endian, t) ->
    if check then check_mem arr idx endian t;
    t
  | Store(arr,idx,vl, endian, t) ->
    if check then (
      check_mem arr idx endian t;
      let tv = infer_ast_internal true vl in
      check_eq tv t "Store of value with type %s performed using a Store of type %s";
    );
    infer_ast_internal false arr

and check_same ?e ?s t1 t2 =
  if t1 <> t2 then
    let probs = match e, s with
      | Some e, _ -> ("\nProblem expression: "^(Pp.ast_exp_to_string e))
      | _, Some s -> ("\nProblem statement: "^(Pp.ast_stmt_to_string s))
      | None, None -> "" in
    terror ("Similar types expected: "^(Pp.typ_to_string t1)^" <> "^(Pp.typ_to_string t2)^probs)

and check_reg t =
  if not (is_integer_type t) then
    terror (Printf.sprintf "Expected integer type, but got %s" (Pp.typ_to_string t))

and check_bool t =
  if t <> Reg 1 then
    terror (Printf.sprintf "Expected bool type, but got %s" (Pp.typ_to_string t))

and check_mem arr idx endian t =
  let ta = infer_ast_internal true arr
  and ti = infer_ast_internal true idx
  and te = infer_ast_internal true endian in
  if te <> reg_1 then terror "Endian must be a boolean";
  if not(is_integer_type ti) then terror "Index must be a register type";
  match ta with
  | Array(i,e) ->
    check_eq ti i "Index type not suitable for indexing into this array. Index has type %s, but array has type %s.";
    check_eq t e "Can't get/put a %s from array with element type of %s";
  | TMem(i,e) ->
    check_eq ti i "Index type not suitable for indexing into this array. Index has type %s, but the memory has type %s.";
    check_mult t e "Can't get/put a %s from memory with element type of %s"
  | _ -> terror "Indexing only allowed from array or mem."

and check_cjmp_direct e =
  if Ast.lab_of_exp e = None then terror "Conditional jump targets must be direct (to a constant address or label)"

let infer_ast = infer_ast_internal false

let typecheck_expression e = ignore(infer_ast_internal true e)

(** Check that conversions between fps and bvs are well-formed.
    Assumes labelled types are accurate without recursing as those will later
    be checked by infer_ast_internal. *)
let rec infer_fp = function
  | Load (arr, idx, endian, t)
  | Store (arr, idx, _, endian, t) ->
    begin match t with
      | Reg _ -> `bv
      | TMem _
      | Array _ -> failwith "unimplemented"
      | Float _ -> `fp
    end
  | BinOp (FP ((FEQ|FLT), _), e1, e2) ->
    begin match (infer_fp e1, infer_fp e2) with
      | `fp, `fp | `any, `fp | `fp, `any -> `bv
      | _ -> terror("Expected FP in BinOp(FP, ...)")
    end
  | BinOp (FP _, e1, e2) ->
    begin match (infer_fp e1, infer_fp e2) with
      | `fp, `fp | `any, `fp | `fp, `any -> `fp
      | _ -> terror("Expected FP in BinOp(FP, ...)")
    end
  | BinOp (_ , e1, e2) ->
    begin match (infer_fp e1, infer_fp e2) with
      | `bv, `bv | `bv, `any | `any, `bv -> `bv
      | _ -> terror("Expected BV in Binop(...)")
    end
  | UnOp (FP (FNAN _, _), _) -> `fp
  (* expects float returns bv *)
  | UnOp (FP ((FISNORM|FISSUB|FISZERO|FISINF|FISNAN|FISNEG|FISPOS|FFTOUBV _|FFTOSBV _|FFTOIEEEBV _), _), e) ->
    begin match infer_fp e with
      | `fp | `any -> `bv
      | _ -> terror("Expected float in UnOp.")
    end
  (* expects bv returns float *)
  | UnOp (FP ((FBVTOUF _|FBVTOSF _|FIEEEBVTOF _), _), e) ->
    begin match infer_fp e with
      | `bv | `any -> `fp
      | _ -> terror("Expected BV in UnOp.")
    end
  (* expects float returns float *)
  | UnOp (FP ((FABS|FNEG|FSQRT|FROUND|FFTOF _), _), e) ->
    begin match infer_fp e with
      | `fp | `any -> `fp
      | _ -> terror("Expected float in UnOp.")
    end
  (* expects bv returns bv *)
  | UnOp (_, e) ->
    begin match infer_fp e with
      | `bv | `any -> `bv
      | _ -> terror("Expected BV in UnOp.")
    end
  | Var _ -> `bv
  | Lab _ | Unknown _ -> `any
  | Int (_, typ) | Cast (_, typ, _) ->
    begin match typ with
      | Reg _ -> `bv
      | TMem _ | Array _ -> failwith "unimplemented"
      | Float _ -> `fp
    end
  | Let (_, e1, e2) ->
    begin match infer_fp e1, infer_fp e2 with
      | `bv, `bv | `bv, `any | `any, `bv -> `bv
      | `fp, `fp | `fp, `any | `any, `fp -> `fp
      | _ -> terror("Float/BV types don't match in Let.")
    end
  | Ite (e1, e2, e3) ->
    if infer_fp e1 <> `bv then terror("BV expected for if expression in Ite.");
    begin match infer_fp e2, infer_fp e3 with
      | `bv, `bv | `bv, `any | `any, `bv -> `bv
      | `fp, `fp | `fp, `any | `any, `fp -> `fp
      | _ -> terror("Expression float/BV types don't match in Ite.");
    end
  | Extract (_, _, exp) ->
    if infer_fp exp <> `bv then terror("Expected a `bv in extract.");
    `bv
  | Concat (e1, e2) ->
    begin match (infer_fp e1, infer_fp e2) with
      | `bv, `bv -> `bv
      | _ -> terror "Can only concat bitvectors."
    end

(* Quick, informal, AST statement type checking.

   XXX: Formal type checking rules!
*)
let typecheck_stmt =
  let infer_te e =
    ignore(infer_fp e);
    infer_ast_internal true e
  in
  function
  | Move(v, e, _) as s ->
    let vt = Var.typ v in
    let et = infer_te e in
    check_same ~s vt et
  | Jmp(e, _) ->
    let et = infer_te e in
    check_reg et
  | CJmp(ce, t1, _) ->
    let et = infer_te ce in
    let t1t = infer_te t1 in
    check_bool et;
    check_reg t1t;
    check_cjmp_direct t1;
  | Halt(e, _) ->
    let et = infer_te e in
    (* Can we return a memory? Does this make sense? *)
    check_reg et
  | Assert(e, _)
  | Assume(e, _) ->
    let et = infer_te e in
    check_bool et
  | Label _
  | Comment _
  | Special _ ->
    ()

let typecheck_prog prog =
  let last_label = ref None in
  let is_asm = function
    | Asm _ -> true
    | _ -> false
  in
  let ts = function
    | Label (_, attrs) as s when List.exists is_asm attrs -> last_label := Some s
    | _ -> ()
  in
  List.iter (fun stmt -> ts stmt;
              try typecheck_stmt stmt
              with TypeError s ->
              match !last_label with
              | Some ls ->
                raise (TypeError (s^"\nat "^Pp.ast_stmt_to_string ls))
              | None -> raise (TypeError s)) prog
