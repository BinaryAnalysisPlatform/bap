open Core_kernel.Std
open Bap.Std
open Bap_primus_sexp

module Index = Bap_primus_lisp_index
module Loc = Bap_primus_lisp_loc
module Source = Bap_primus_lisp_source

module Id = Source.Id
module Eq = Source.Eq
type ('a,'i,'e) interned = ('a,'i,'e) Index.interned = {
  data : 'a;
  id : 'i;
  eq : 'e;
} [@@deriving compare, sexp]

type 'a indexed = ('a,Id.t,Eq.t) interned [@@deriving compare]

type typ =
  | Any
  | Symbol
  | Name of string
  | Type of int [@@deriving sexp, compare]
type 'a term = {exp : 'a; typ : typ} [@@deriving compare]
type word = int64 term indexed [@@deriving compare]
type var = string term indexed [@@deriving compare]
type sym = string indexed [@@deriving compare]
type loc = Loc.t

type error = ..
exception Fail of error


type tree = Source.tree
type token = Source.token =
  | Atom of string
  | List of tree list


type ast = exp indexed
and exp =
  | Int of word
  | Var of var
  | Sym of sym
  | Ite of ast * ast * ast
  | Let of var * ast * ast
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
