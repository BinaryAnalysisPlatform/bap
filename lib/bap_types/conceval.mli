(** Concrete Evaluator.
    This module is intentionally undocumented and will be refined in a
    near future.
*)

open Core_kernel.Std
open Bap.Std

type state
with bin_io, compare, sexp

type memory
with bin_io, compare, sexp

type t =
  | BV  of Bitvector.t
  | Mem of memory
  | Un  of string * typ
with bin_io, compare, sexp


module Memory : sig
  val empty : t
  val load  : mem:t -> idx:t -> endian -> size -> t option
  val store : mem:t -> idx:t -> data:t -> endian -> size -> t
end

module State : sig
  val empty : state
  val move  : state -> key:var -> data:t -> state
  val peek  : state -> var -> t option
end

val eval_exp   : state -> exp -> t
val eval_stmt  : state -> stmt -> state * t option
val eval_stmts : state -> stmt list -> state * t option

include Regular with type t := t
