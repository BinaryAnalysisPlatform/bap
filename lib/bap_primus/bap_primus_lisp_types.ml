open Core_kernel.Std
open Bap.Std
open Bap_primus_sexp

module Index = Bap_primus_lisp_index
module Loc = Bap_primus_lisp_loc
module Source = Bap_primus_lisp_source

module Id = Source.Id
module Eq = Source.Eq

type bop = Add | Sub | Mul | Div | Mod | Divs | Mods
         | Lsl | Lsr | Asr | Land | Lior | Lxor | Cat
         | Equal | Less | And | Or
[@@deriving sexp]
type uop = Lneg | Lnot | Not [@@deriving sexp]
type typ = Word | Type of int [@@deriving compare, sexp]
type 'a term = {exp : 'a; typ : typ}[@@deriving compare, sexp]
type word = int64 term [@@deriving compare, sexp]
type var = string term[@@deriving compare, sexp]
type loc = Loc.t [@@deriving compare, sexp_of]

type error = ..
exception Fail of error

type ('a,'i,'e) interned = ('a,'i,'e) Index.interned = {
  data : 'a;
  id : 'i;
  eq : 'e;
}

type tree = Source.tree
type token = Source.token =
  | Atom of string
  | List of tree list

type 'a indexed = ('a,Id.t,Eq.t) interned

type ast = exp indexed
and exp =
  | Int of word
  | Var of var
  | Ite of ast * ast * ast
  | Let of var * ast * ast
  | Ext of ast * ast * ast
  | Bop of bop * ast * ast
  | Uop of uop * ast
  | App of binding * ast list
  | Seq of ast list
  | Set of var * ast
  | Rep of ast * ast
  | Msg of fmt list * ast list
  | Err of string
and fmt =
  | Lit of string
  | Pos of int
and binding =
  | Dynamic of string
  | Static of var list * ast
