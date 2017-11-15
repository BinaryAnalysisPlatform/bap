open Core_kernel.Std
open Bap.Std
open Bap_primus_sexp

type bop = Add | Sub | Mul | Div | Mod | Divs | Mods
         | Lsl | Lsr | Asr | Land | Lior | Lxor | Cat
         | Equal | Less | And | Or
[@@deriving sexp]
type uop = Lneg | Lnot | Not [@@deriving sexp]
type sexp = Sexp.t = Atom of string | List of sexp list[@@deriving sexp]
type typ = Word | Type of int [@@deriving compare, sexp]
type 'a scalar = {data : 'a; typ : typ}[@@deriving compare, sexp]
type word = int64 scalar [@@deriving compare, sexp]
type var = string scalar[@@deriving compare, sexp]
type pos = Bap_primus_lisp_loc.pos [@@deriving compare, sexp_of]
type loc = Bap_primus_lisp_loc.t [@@deriving compare, sexp_of]

type 'a arg = {
  arg : 'a;
  pos : pos;
}

type exp =
  | Int of word arg
  | Var of var arg
  | Ite of (exp * exp * exp) arg
  | Let of (var * exp * exp) arg
  | Ext of (exp * exp * exp) arg
  | Bop of (bop * exp * exp) arg
  | Uop of (uop * exp) arg
  | App of (string * exp list) arg
  | Seq of (exp list) arg
  | Set of (var * exp) arg
  | Rep of (exp * exp) arg
  | Msg of (fmt list * exp list) arg
  | Err of string arg
and fmt = Lit of string | Exp of exp | Pos of int


type parse_error = ..

exception Parse_error of parse_error * sexp
