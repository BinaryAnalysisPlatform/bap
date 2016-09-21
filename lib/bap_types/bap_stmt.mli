open Core_kernel.Std
open Regular.Std
open Bap_common
open Bap_bil

include Regular.S with type t := stmt

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

module Stmts_pp : Printable.S with type t = stmt list
module Stmts_data : Data.S with type t = stmt list
