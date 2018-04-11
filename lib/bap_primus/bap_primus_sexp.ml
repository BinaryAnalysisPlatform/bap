open Core_kernel
open Bap.Std
open Format

open Bap_primus_types

let sexp_of_tid t = Sexp.Atom (Tid.name t)
let sexp_of_var v = Sexp.Atom (Var.to_string v)


let string_of_byte w = sprintf "%02x" @@ ok_exn (Word.to_int w)
let string_of_word w = asprintf "%a" Word.pp_hex_full w
let sexp_of_word w = Sexp.Atom (string_of_word w)
let string_of_value = Bap_primus_value.to_string
let sexp_of_value = Bap_primus_value.sexp_of_t
let sexp_of_byte w = Sexp.Atom (string_of_byte w)
let sexp_of_binding (v,r) = [%sexp ((v : var), (r : value))]
let sexp_of_values (x,y) = [%sexp ((x:value),(y:value))]
let sexp_of_move (a,w) = [%sexp ((a : word), (w : word))]
let sexps atoms = Sexp.List (List.map ~f:(fun x -> Sexp.Atom x) atoms)

let sexp_of_size size = sexp_of_int (Size.in_bytes size)

let rec sexp_of_exp = function
  | Bil.Load (e1,e2,_,`r8) ->
    Sexp.List (Sexp.Atom "load" :: sexps_of_exps [e1;e2])
  | Bil.Load (e1,e2,endian,size) ->
    Sexp.List [
      Sexp.Atom "load";
      sexp_of_exp e1;
      sexp_of_exp e2;
      sexp_of_endian endian;
      sexp_of_size size
    ]
  | Bil.Store (e1,e2,e3,_,`r8) ->
    Sexp.List (Sexp.Atom "store" :: sexps_of_exps [e1;e2;e3])
  | Bil.Store (e1,e2,e3,endian,size) ->
    Sexp.List [
      Sexp.Atom "store";
      sexp_of_exp e1;
      sexp_of_exp e2;
      sexp_of_exp e3;
      sexp_of_endian endian;
      sexp_of_size size
    ]
  | Bil.BinOp (op, x, y) ->
    Sexp.List [
      Sexp.Atom (Bil.string_of_binop op);
      sexp_of_exp x;
      sexp_of_exp y;
    ]

  | Bil.UnOp (op,x) ->
    Sexp.List [
      Sexp.Atom (Bil.string_of_unop op);
      sexp_of_exp x;
    ]
  | Bil.Var x -> sexp_of_var x
  | Bil.Int x -> sexp_of_word x
  | Bil.Cast (ct,n,x) ->
    Sexp.List [
      Sexp.Atom (Bil.string_of_cast ct);
      sexp_of_int n;
      sexp_of_exp x;
    ]
  | Bil.Let (v,x,y) ->
    Sexp.List [
      Sexp.Atom "let";
      Sexp.List [Sexp.List [sexp_of_var v; sexp_of_exp x]];
      sexp_of_exp y;
    ]
  | Bil.Ite (c,x,y) ->
    Sexp.List [
      Sexp.Atom "ite";
      sexp_of_exp x;
      sexp_of_exp y;
    ]
  | Bil.Unknown (v,x) -> Sexp.Atom "bot"
  | Bil.Extract (hi,lo,x) ->
    Sexp.List [
      Sexp.Atom "extract";
      sexp_of_int hi;
      sexp_of_int lo;
      sexp_of_exp x;
    ]
  | Bil.Concat (x,y) ->
    Sexp.List [
      Sexp.Atom "concat";
      sexp_of_exp x;
      sexp_of_exp y;
    ]
and sexps_of_exps = List.map ~f:sexp_of_exp
