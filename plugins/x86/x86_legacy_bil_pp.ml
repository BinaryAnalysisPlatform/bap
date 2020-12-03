(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved.*)
(** Pretty printing

    @todo Write .mli
*)

module Bil = X86_legacy_bil
open Big_int_Z
open Bil
open Bil.Type
open Core_kernel

module Arithmetic = X86_legacy_bil_arithmetic
module Big_int_convenience = X86_legacy_bil_big_int_convenience

open Big_int_convenience

module VH = Var.Table
module F = Format

let output_varnums = ref true

let many_parens = ref false

let rec typ_to_string = function
  | Reg 1 -> "bool"
  | Reg 8 -> "u8"
  | Reg 16 -> "u16"
  | Reg 32 -> "u32"
  | Reg 64 -> "u64"
  | Reg n -> Printf.sprintf "u%u" n
  | TMem (idx,e) -> typ_to_string idx ^ "?" ^ typ_to_string e
  | Array(idx,e) -> typ_to_string idx ^ "!" ^ typ_to_string e
  | fp when Poly.(=) fp fp_16 -> "f16"
  | fp when Poly.(=) fp fp_32 -> "f32"
  | fp when Poly.(=) fp fp_64 -> "f64"
  | fp when Poly.(=) fp fp_80 -> "f80"
  | fp when Poly.(=) fp fp_128 -> "f128"
  | Float {exp_bits; sig_bits} -> Printf.sprintf "f%u_%u" exp_bits sig_bits

let ct_to_string = function
  | CAST_UNSIGNED  -> "pad"
  | CAST_SIGNED -> "extend"
  | CAST_HIGH -> "high"
  | CAST_LOW -> "low"

let fbinop_to_string = function
  | FADD -> "+."
  | FSUB -> "-."
  | FMUL -> "*."
  | FDIV -> "/."
  | FREM -> "%."
  | FMIN -> "fmin"
  | FMAX -> "fmax"
  | FLE -> "<=."
  | FLT -> "<."
  | FEQ -> "==."

let roundmode_to_string = function
  | RNE -> "RNE"
  | RTZ -> "RTZ"
  | RTP -> "RTP"
  | RTN -> "RTN"
  | RNA -> "RNA"

let funop_to_string = function
  | FABS -> "fabs"
  | FNEG -> "fneg"
  | FSQRT -> "fsqrt"
  | FROUND -> "fround"
  | FISNORM -> "fisnorm"
  | FISSUB -> "fissub"
  | FISZERO -> "fiszero"
  | FISINF -> "fisinf"
  | FISNAN -> "fisnan"
  | FISNEG -> "fisneg"
  | FISPOS -> "fispos"

  | FFTOUBV bits -> Printf.sprintf "ftoubv(%d)" bits
  | FFTOSBV bits -> Printf.sprintf "ftosbv(%d)" bits
  | FFTOIEEEBV bits -> Printf.sprintf "fftoIEEE(%d)" bits
  | FBVTOUF {exp_bits; sig_bits} -> Printf.sprintf "bvtouf(%d, %d)" exp_bits sig_bits
  | FBVTOSF {exp_bits; sig_bits} -> Printf.sprintf "bvtosf(%d, %d)" exp_bits sig_bits
  | FFTOF {exp_bits; sig_bits} -> Printf.sprintf "ftof(%d, %d)" exp_bits sig_bits
  | FIEEEBVTOF {exp_bits; sig_bits} -> Printf.sprintf "fIEEEtof(%d, %d)" exp_bits sig_bits
  | FNAN {exp_bits; sig_bits} -> Printf.sprintf "fnan(%d, %d)" exp_bits sig_bits


let binop_to_string = function
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | SDIVIDE -> "$/"
  | MOD -> "%"
  | SMOD -> "$%"
  | LSHIFT -> "<<"
  | RSHIFT -> ">>"
  | ARSHIFT -> "$>>"
  | AND -> "&"
  | OR -> "|"
  | XOR -> "^"
  | EQ -> "=="
  | NEQ -> "<>"
  | LT -> "<"
  | LE -> "<="
  | SLT -> "$<"
  | SLE -> "$<="
  | FP (op, rm) ->
    Printf.sprintf "%s:%s" (fbinop_to_string op) (roundmode_to_string rm)

let unop_to_string = function
  | NEG -> "-"
  | NOT -> "~"
  | FP (op, rm) ->
    Printf.sprintf "%s:%s " (funop_to_string op) (roundmode_to_string rm)

let option_may ~f = function
  | Some x -> (f x)
  | None -> ()

let reasonable_size_varctx = 10000
let printed_varctx_warning = ref false
type varctx = string VH.t * (string,unit) Hashtbl.t

let var_to_string ?ctx (Var.V(id,name,t) as v) =
  match ctx with
  | None ->
    name ^ "_" ^ string_of_int id ^ ":" ^ typ_to_string t
  | Some(vars,names) ->
    if (not !printed_varctx_warning) &&
       (Hashtbl.length names > reasonable_size_varctx ||
        VH.length vars > reasonable_size_varctx) then (
      printed_varctx_warning := true);
    (*        Logs.tbd_s "varctx is getting very large"); *)
    match VH.find vars v with
    | Some s -> s
    | None ->
      let rec trystring (s, `F next) =
        if Hashtbl.mem names s then
          trystring (next s)
        else (
          let s' = s ^":"^ typ_to_string t in
          VH.add_exn vars ~key:v ~data:s';
          Hashtbl.add_exn names ~key:s ~data:();
          s')
      in
      let rec more x = (x^"_", `F more) in
      trystring (name, `F (fun _ -> (name ^ "_" ^ string_of_int id, `F more)))


class pp ft =
  let pp = F.pp_print_string ft
  and pc = F.pp_print_char ft
  and space = Format.pp_print_space ft
  and break () = Format.pp_print_break ft 0 1
  and printf = Format.fprintf ft
  and opn  = F.pp_open_hovbox ft
  and cls = F.pp_close_box ft in
  let comma () = pp ","; space() in
  let vctx = (VH.create ~size:100 (), Hashtbl.Using_hashable.create ~hashable:Hashtbl.Poly.hashable ~size:100 ()) in
  let commalist f = function
    | [] -> ()
    | e::[] -> f e
    | hd::others ->
      f hd;
      List.iter ~f:(fun e -> pc ','; space (); f e) others
  in
  object (self)


    method var v =
      if !output_varnums then
        pp (var_to_string v)
      else
        pp (var_to_string ~ctx:vctx v)

    method vars = commalist self#var

    method typ t = pp (typ_to_string t)


    method attrs a = List.iter ~f:(fun a -> space();self#attr a) a

    method attr = function
      | Asm s -> pp "@asm \""; pp s; pp "\""
      | Address a -> printf "@address \"0x%s\"" (big_int_to_hex a);
      | Target l -> pp "@target \""; self#label l; pp "\""
      | Liveout -> pp "@set \"liveout\""
      | StrAttr s -> pp "@str \""; pp s; pc '\"'
      | NamedStrAttr (n, s) -> pp "@namedstr \""; pp n; pc '\"'; space (); pc '\"'; pp s; pc '\"'
      | Context {name=s; mem=mem; value=v; index=i; t=Reg bits; usage=u; taint=Taint t} ->
        let ustr = match u with
          | RD -> "rd" | RW -> "rw" | WR -> "wr"
        in
        let ts = string_of_int t in
        (*if t = Taint then "tainted" else "untainted" in*)
        let ind = if mem then "[0x"^(big_int_to_hex i)^"]" else "" in
        pp "@context "; pc '"'; pp (s^ind); pc '"'; pp (" = 0x"^(big_int_to_hex v)^ ", " ^ ts
                                                        ^", u"
                                                        ^ (string_of_int bits)
                                                        ^", "
                                                        ^ustr)
      | Context _ ->
        failwith "Contexts only specify register types"
      | ThreadId i -> pp "@tid \""; pp (string_of_int i); pp "\""
      | ExnAttr _ (* we could try to print something using Printexc.to_string *)
      | InitRO -> pp "@set \"initro\""
      | Synthetic -> pp "@set \"synthetic\""
      | SpecialBlock -> pp "@set \"specialblock\""
      | TaintIntro(tid, src_name, offset) ->
        pp "@taint_intro "; pp (string_of_int tid); pp ", "; pc '"';
        pp src_name; pp "\", "; pp (string_of_int offset)

    method du {Var.defs; Var.uses} =
      (match defs with
       | _::_ ->
         space ();
         pp "defs";
         space ();
         self#vars defs
       | _ -> ());

      (match uses with
       | _::_ ->
         space ();
         pp "uses";
         space ();
         self#vars uses
       | _ -> ())

    method label = function
      | Name s -> pp "label "; pp s
      | Addr x -> printf "addr 0x%s" (big_int_to_hex x)

    method int i t =
      let (is, i) = Arithmetic.to_sbig_int (i,t), Arithmetic.to_big_int (i,t) in
      match (is, t) with
      | (bi, Reg 1) when bi_is_zero bi -> pp "false"
      | (bi, Reg 1) when bi_is_minusone bi -> pp "true"
      | (bi,t) ->
        if (abs_big_int bi) <% bia
        then pp (string_of_big_int bi)
        else pp ("0x"^(big_int_to_hex (Arithmetic.to_big_int (i,t))));
        pp ":"; self#typ t


    (* prec tells us how much parenthization we need. 0 means it doesn't need
       to be parenthesized. Larger numbers means it has higher precedence.
       Maximum prec before paretheses are added are as follows:
       5 Let
       7 Ite
       10 Store
       12 Concat
       15 Extract
       20 OR
       30 XOR
       40 AND
       50 EQ NEQ
       60 LT SLT SLE LE
       70 LSHIFT RSHIFT ARSHIFT
       80 PLUS MINUS
       90 TIMES DIVIDE SDIVIDE MOD SMOD
       100 UMINUS NOT
       110 Get
       Because we don't distinguish precedence to the right or left, we will
       always overparethesise expressions such as:
       let x = y in x + let x = 2:reg32_t in x
    *)
    method ast_exp ?(prec=0) e =
      let lparen bind = if !many_parens || bind < prec then pp "("
      and rparen bind = if !many_parens || bind < prec then pp ")"
      and binop_prec = function
        | (FP _:Type.binop_type) -> 10
        | OR -> 20
        | XOR -> 30
        | AND -> 40
        | EQ | NEQ -> 50
        | LT | SLT | SLE | LE -> 60
        | LSHIFT | RSHIFT | ARSHIFT -> 70
        | PLUS | MINUS -> 80
        | TIMES | DIVIDE | SDIVIDE | MOD | SMOD -> 90
      in
      opn 0;
      (match e with
       | Ast.Load(arr,idx,edn,t) ->
         lparen 110;
         self#ast_exp ~prec:110 arr;
         pp "["; self#ast_exp idx; comma(); self#ast_endian edn; pp "]";
         (* FIXME: check type of arr *)
         pp ":"; self#typ t;
         rparen 110
       | Ast.Store(arr,idx,vl, edn, t) ->
         lparen 10;
         self#ast_exp ~prec:10 arr;
         pp " with"; space();
         pp "["; self#ast_exp idx;
         comma(); self#ast_endian edn;
         pp "]:"; self#typ t;
         pp " ="; space();
         self#ast_exp ~prec:10 vl;
         rparen 10;
       | Ast.Ite(c,x,y) ->
         lparen 7;
         pp "if";
         space ();
         self#ast_exp ~prec:7 c;
         space ();
         pp "then";
         space ();
         self#ast_exp ~prec:7 x;
         space ();
         pp "else";
         space ();
         self#ast_exp ~prec:7 y;
         rparen 7
       | Ast.Extract(h, l, e) ->
         pp "extract:";
         pp (string_of_big_int h);
         pc ':';
         pp (string_of_big_int l);
         pc ':';
         pc '[';
         self#ast_exp e;
         pc ']';
       | Ast.Concat(le, re) ->
         pp "concat:";
         pc '[';
         break ();
         self#ast_exp le;
         pp "][";
         break ();
         self#ast_exp re;
         pc ']'
       | Ast.BinOp(b,x,y) ->
         let p = binop_prec b in
         lparen p;
         self#ast_exp ~prec:p x;
         pp " "; pp (binop_to_string b); space();
         self#ast_exp ~prec:(p+1) y;
         rparen p
       | Ast.UnOp(u, x) ->
         lparen 100;
         pp (unop_to_string u); self#ast_exp ~prec:100 x;
         rparen 100
       | Ast.Var v ->
         self#var v
       | Ast.Lab s ->
         (* FIXME: quote s? *)
         pp "\""; pp s; pp "\"";
       | Ast.Int(i,t) ->
         self#int i t
       | Ast.Cast(ct,t,e) ->
         pp (ct_to_string ct);
         pp ":"; self#typ t;
         pp "("; self#ast_exp e; pp ")"
       | Ast.Let(v,e1,e2) ->
         lparen 5;
         pp "let "; self#var v; pp " :=";
         opn 2; space();
         self#ast_exp ~prec:5 e1; space();
         cls();
         pp "in"; space();
         self#ast_exp ~prec:5 e2;
         rparen 5
       | Ast.Unknown(s,t) ->
         pp "unknown \""; pp s; pp "\":"; self#typ t
      );
      cls();

    method ast_endian = function
      | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
        pp "e_little";
      | Ast.Int(bi, Reg 1) when bi_is_one bi ->
        pp "e_big"
      | x -> self#ast_exp x

    method ast_stmt s =
      opn 2;
      (match s with
       | Ast.Move(v, e, a) ->
         self#var v;
         pp " ="; space();
         self#ast_exp e;
         self#attrs a
       | Ast.Jmp(e,a) ->
         pp "jmp ";
         self#ast_exp e;
         self#attrs a;
       | Ast.CJmp(c,t,a) ->
         pp "if"; space();
         self#ast_exp c; pp " goto ";
         self#ast_exp t;
         self#attrs a
       | Ast.Label(l,a) ->
         self#label l;
         self#attrs a
       | Ast.Halt(e,a) ->
         pp "halt ";
         self#ast_exp e;
         self#attrs a
       | Ast.Assert(e,a) ->
         pp "assert ";
         self#ast_exp e;
         self#attrs a
       | Ast.Assume(e,a) ->
         pp "assume ";
         self#ast_exp e;
         self#attrs a
       | Ast.Comment(s,a) ->
         pp "/*";
         pp s;
         pp "*/";
         self#attrs a
       | Ast.Special(s,du,a) ->
         pp "special";
         option_may ~f:self#du du;
         space ();
         pc '"';
         pp s;
         pc '"';
         self#attrs a);
      cls();

    method ast_program p =
      Format.pp_open_hvbox ft 0;
      List.iter ~f:(fun x -> self#ast_stmt x; space()) p;
      Format.pp_force_newline ft ();
      cls();

    method close =
      Format.pp_print_newline ft ();

  end

(* Argh, these should all be locals, but then ocaml's type system won't let
   pp2string be polymorphic *)
let buf = Buffer.create 57
let ft =
  let out_string = Caml.Buffer.add_substring buf
  and out_spaces _ = Caml.Buffer.add_char buf ' ' in
  let ft = Format.formatter_of_buffer buf in
  Format.pp_set_formatter_out_functions ft {
    Format.out_string; out_flush = ignore;
    out_spaces;
    out_newline = ignore;
    out_indent = out_spaces;
  };
  ft

let pp2string_with_pp pp f v =
  Format.pp_open_box ft 0;
  f pp v;
  Format.pp_print_flush ft ();
  let s = Buffer.contents buf in
  Buffer.reset buf;
  s

let pp2string f v =
  let strpp = new pp ft in
  pp2string_with_pp strpp f v

(** Create a context for use with *_to_string_in_varctx functions. *)
let make_varctx () =
  new pp ft

(* These functions will not remember variable names across separate
   invocations *)
let label_to_string = pp2string (fun p -> p#label)
let ast_exp_to_string = pp2string (fun p -> p#ast_exp ~prec:0)
let ast_stmt_to_string = pp2string (fun p -> p#ast_stmt)
let ast_prog_to_string p = String.concat ~sep:"\n" (List.map ~f:ast_stmt_to_string p)
