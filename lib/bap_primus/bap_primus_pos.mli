open Core_kernel.Std
open Bap.Std
open Monads.Std

type nil = Nil
type top = program

type ('a,'b) level = {
  me : 'a term;
  up : 'b;
}

type level3 = (top,nil) level
type level2 = (sub,level3) level
type 'a level1 = ('a,level2) level
type 'a level0 = ('a,blk level1) level

type t =
  | Top of level3
  | Sub of level2
  | Arg of arg level1
  | Blk of blk level1
  | Phi of phi level0
  | Def of def level0
  | Jmp of jmp level0
[@@deriving sexp_of]

val to_string : t -> string

val tid : t -> tid
val get : 'a tag -> t -> 'a option
val next : t -> ('p,'t) cls -> 't term -> (t,Bap_primus_exn.t) Monad.Result.result
