(** Extends [stmt] interface  *)
open Core_kernel.Std
open Bap_common
open Bap_bil

include Regular with type t := stmt

(** [pp_stmts] pretty prints a sequence of statements.  *)
val pp_stmts : Format.formatter -> stmt list -> unit
