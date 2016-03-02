open Core_kernel.Std
open Bap.Std
open Bil.Types

module P = Stmt_piqi
open Type

let casttype_to_piqi = function
  | UNSIGNED -> `cast_unsigned
  | SIGNED -> `cast_signed
  | HIGH -> `cast_high
  | LOW -> `cast_low

let casttype_of_piqi = function
  | `cast_unsigned -> UNSIGNED
  | `cast_signed -> SIGNED
  | `cast_high -> HIGH
  | `cast_low -> LOW

let unop_to_piqi : 'a -> Stmt_piqi.unop_type = function
  | NEG -> `uneg
  | NOT -> `unot

let unop_of_piqi = function
  | `uneg -> NEG
  | `unot -> NOT

let binop_to_piqi : 'a -> Stmt_piqi.binop_type = function
  | PLUS -> `plus
  | MINUS -> `minus
  | TIMES -> `times
  | DIVIDE -> `divide
  | SDIVIDE -> `sdivide
  | MOD -> `modbop
  | SMOD -> `smod
  | LSHIFT -> `lshift
  | RSHIFT -> `rshift
  | ARSHIFT -> `arshift
  | AND -> `andbop
  | OR -> `orbop
  | XOR -> `x_or
  | EQ -> `eq
  | NEQ -> `neq
  | LT -> `lt
  | LE -> `le
  | SLT -> `slt
  | SLE -> `sle

let binop_of_piqi = function
  | `plus -> PLUS
  | `minus -> MINUS
  | `times -> TIMES
  | `divide -> DIVIDE
  | `sdivide -> SDIVIDE
  | `modbop -> MOD
  | `smod -> SMOD
  | `lshift -> LSHIFT
  | `rshift -> RSHIFT
  | `arshift -> ARSHIFT
  | `andbop -> AND
  | `orbop -> OR
  | `x_or -> XOR
  | `eq -> EQ
  | `neq -> NEQ
  | `lt -> LT
  | `le -> LE
  | `slt -> SLT
  | `sle -> SLE

let rec type_to_piqi : typ -> Stmt_piqi.typ = function
  | Imm s ->  `imm s
  | Mem (t, t') -> `mem {Stmt_piqi.Mem.index_type = t; element_type = t';}

let rec type_of_piqi = function
  | `imm n -> Imm n
  | `mem {P.Mem.index_type; element_type} -> Mem (index_type, element_type)


let var_to_piqi v =
  let module P = Stmt_piqi in {
    P.Var.name = Var.name v;
    P.Var.id  = Var.index v;
    P.Var.typ = type_to_piqi (Var.typ v);
    P.Var.tmp = Var.is_virtual v;
  }

let var_of_piqi { P.Var.name; id; typ; tmp} =
  let module P = Stmt_piqi in
  let v = Var.create ~is_virtual:tmp name (type_of_piqi typ) in
  Var.with_index v id

let endianness_to_piqi : endian -> Stmt_piqi.endian = function
  | LittleEndian -> `little_endian
  | BigEndian -> `big_endian

let endianness_of_piqi = function
  | `little_endian -> LittleEndian
  | `big_endian -> BigEndian

let rec exp_to_piqi : exp -> Stmt_piqi.expr =
  function
  | Load (m, i, e, s) ->
    let m = exp_to_piqi m in
    let i = exp_to_piqi i in
    let e = endianness_to_piqi e in
    `load {P.Load.memory=m; address=i; endian=e; size=s;}
  | Store (m, i, v, e, size) ->
    let m = exp_to_piqi m in
    let i = exp_to_piqi i in
    let v = exp_to_piqi v in
    let e = endianness_to_piqi e in
    `store {P.Store.memory=m; address=i; value=v; endian=e; size;}
  | BinOp (bop, e1, e2) ->
    let bop = binop_to_piqi bop in
    let e1 = exp_to_piqi e1 in
    let e2 = exp_to_piqi e2 in
    `binop {P.Binop.op=bop; lexp=e1; rexp=e2;}
  | UnOp (uop, e) ->
    let uop = unop_to_piqi uop in
    let e = exp_to_piqi e in
    `unop {P.Unop.op=uop; exp=e}
  | Var v ->
    `var (var_to_piqi v)
  | Int v ->
    `inte {P.Inte.int = Bitvector.to_string v;}
  | Cast (ct, size, e) ->
    let ct = casttype_to_piqi ct in
    let e = exp_to_piqi e in
    `cast {P.Cast.cast_type=ct; size; exp=e}
  | Let (v, e, e') ->
    let v = var_to_piqi v in
    let e = exp_to_piqi e in
    let e' = exp_to_piqi e' in
    `let_exp {P.Let_exp.bound_var=v; definition=e; open_exp=e'}
  | Unknown (s, t) ->
    let t = type_to_piqi t in
    `unknown {P.Unknown.descr=s; typ=t}
  | Ite (e, te, fe) ->
    let e = exp_to_piqi e in
    let te = exp_to_piqi te in
    let fe = exp_to_piqi fe in
    `ite {P.Ite.condition=e; iftrue=te; iffalse=fe}
  | Extract (h, l, e) ->
    let e = exp_to_piqi e in
    `extract {P.Extract.hbit=h; lbit=l; exp=e}
  | Concat (e1, e2) ->
    let e1 = exp_to_piqi e1 in
    let e2 = exp_to_piqi e2 in
    `concat {P.Concat.lexp=e1; rexp=e2}

let rec exp_of_piqi = function
  | `load {P.Load.memory; address; endian; size} ->
    let m = exp_of_piqi memory in
    let i = exp_of_piqi address in
    let e = endianness_of_piqi endian in
    Load (m, i, e, size)
  | `store {P.Store.memory; address; value; endian; size} ->
    let m = exp_of_piqi memory in
    let i = exp_of_piqi address in
    let v = exp_of_piqi value in
    let e = endianness_of_piqi endian in
    Store (m, i, v, e, size)
  | `binop {P.Binop.op; lexp; rexp} ->
    let bop = binop_of_piqi op in
    let e1 = exp_of_piqi lexp in
    let e2 = exp_of_piqi rexp in
    BinOp (bop, e1, e2)
  | `unop {P.Unop.op; exp} ->
    let uop = unop_of_piqi op in
    let e = exp_of_piqi exp in
    UnOp (uop, e)
  | `var v ->
    Var (var_of_piqi v)
  | `inte {P.Inte.int=s} ->
    Int (Bitvector.of_string s)
  | `cast {P.Cast.cast_type; size; exp} ->
    let ct = casttype_of_piqi cast_type in
    let e = exp_of_piqi exp in
    Cast (ct, size, e)
  | `let_exp {P.Let_exp.bound_var; definition; open_exp} ->
    let v = var_of_piqi bound_var in
    let d = exp_of_piqi definition in
    let e = exp_of_piqi open_exp in
    Let (v, d, e)
  | `unknown {P.Unknown.descr; typ} ->
    let t = type_of_piqi typ in
    Unknown (descr, t)
  | `ite {P.Ite.condition; iftrue; iffalse} ->
    let cond = exp_of_piqi condition in
    let te = exp_of_piqi iftrue in
    let fe = exp_of_piqi iffalse in
    Ite (cond, te, fe)
  | `extract {P.Extract.hbit; lbit; exp} ->
    let e = exp_of_piqi exp in
    Extract (hbit, lbit, e)
  | `concat {P.Concat.lexp; rexp} ->
    let e1 = exp_of_piqi lexp in
    let e2 = exp_of_piqi rexp in
    Concat (e1, e2)

let rec stmt_to_piqi : stmt -> Stmt_piqi.stmt = function
  | Move (v, e) ->
    let v = var_to_piqi v in
    let e = exp_to_piqi e in
    `move {P.Move.lvar=v; rexp=e}
  | Jmp targ ->
    let targ = exp_to_piqi targ in
    `jmp {P.Jmp.target=targ}
  | Special s -> `special s
  | While (e, stmts) ->
    let e = exp_to_piqi e in
    let stmts = stmts_to_piqi stmts in
    `while_stmt {P.While_stmt.cond=e; loop_body=stmts}
  | If (e, then_branch, else_branch) ->
    let e = exp_to_piqi e in
    let then_branch = stmts_to_piqi then_branch in
    let else_branch = stmts_to_piqi else_branch in
    `if_stmt {P.If_stmt.cond=e; true_branch=then_branch; false_branch=else_branch}
  | CpuExn n -> `cpuexn {P.Cpuexn.errnum=n}

and stmts_to_piqi l = List.map ~f:stmt_to_piqi l

let rec stmt_of_piqi = function
  | `move {P.Move.lvar; rexp} ->
    let v = var_of_piqi lvar in
    let e = exp_of_piqi rexp in
    Move (v, e)
  | `jmp {P.Jmp.target} ->
    let t = exp_of_piqi target in
    Jmp t
  | `special s -> Special s
  | `while_stmt {P.While_stmt.cond; loop_body} ->
    let e = exp_of_piqi cond in
    let b = stmts_of_piqi loop_body in
    While (e, b)
  | `if_stmt {P.If_stmt.cond; true_branch; false_branch} ->
    let e = exp_of_piqi cond in
    let then_branch = stmts_of_piqi true_branch in
    let else_branch = stmts_of_piqi false_branch in
    If (e, then_branch, else_branch)
  | `cpuexn {P.Cpuexn.errnum} -> CpuExn errnum

and stmts_of_piqi l = List.map ~f:stmt_of_piqi l

open Stmt_piqi_ext

type fmt = [ `json | `pb | `piq | `pib | `xml ] [@@deriving sexp, enumerate]
type out_fmt = [fmt | `json_pretty | `xml_pretty]

let loads f g fmt s = f (g s fmt)
let dumps g f fmt x = f (g x) (fmt : fmt :> out_fmt)

let bil_of_string  = loads stmts_of_piqi parse_stmt_list
let stmt_of_string = loads stmt_of_piqi parse_stmt
let exp_of_string  = loads exp_of_piqi parse_expr
let string_of_bil  = dumps stmts_to_piqi gen_stmt_list
let string_of_stmt = dumps stmt_to_piqi gen_stmt
let string_of_exp  = dumps exp_to_piqi gen_expr
let piqi_of_exp = exp_to_piqi
let piqi_of_var = var_to_piqi
