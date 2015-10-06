(** Extends [stmt] interface  *)
open Core_kernel.Std
open Bap_common
open Bap_bil

include Regular with type t := stmt

(** [pp_stmts] pretty prints a sequence of statements.  *)
val pp_stmts : Format.formatter -> stmt list -> unit

module Stmt : sig
  val move : var -> exp -> stmt
  val jmp : exp -> stmt
  val special : string -> stmt
  val while_ : exp -> stmt list -> stmt
  val if_ : exp -> stmt list -> stmt list -> stmt
  val cpuexn : int -> stmt
end

module Infix : sig
  val (:=) : var -> exp -> stmt
end

module Stmts_pp : Printable with type t = stmt list
