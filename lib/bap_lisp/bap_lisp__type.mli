open Core_kernel
open Bap_core_theory

open Bap_lisp__types

module Context = Bap_lisp__context

type t = typ [@@deriving compare, sexp]
type context = Context.t
type signature = {
  args : typ list;
  rest : typ option;
  ret  : typ;
}

type read_error = Empty | Not_sexp | Bad_sort


val symbol_size : int
val read : string -> (t,read_error) result
val bool : t
val word : int -> t
val any : t
val sym : t
val var : string -> t

val signature : ?rest:t -> t list -> t -> signature


val pp : Format.formatter -> t -> unit

module Check : sig
  val sort : t -> 'a sort -> bool
end


(* module Spec : sig
 *   type t
 *
 *   type parameters = [
 *     | `All of t
 *     | `Gen of t list * t
 *     | `Tuple of t list
 *   ]
 *
 *   val any : t
 *   val var : string -> t
 *   val sym : t
 *   val int : t
 *   val bool : t
 *   val byte : t
 *   val word : int -> t
 *   val a : t
 *   val b : t
 *   val c : t
 *   val d : t
 *
 *   val tuple : t list -> [`Tuple of t list]
 *   val all : t -> [`All of t]
 *   val one : t -> [`Tuple of t list]
 *   val unit : [`Tuple of t list]
 *   val (//) : [`Tuple of t list] -> [`All of t] -> parameters
 *   val (@->) : [< parameters] -> t -> signature
 * end *)

include Comparable.S_plain with type t := t
