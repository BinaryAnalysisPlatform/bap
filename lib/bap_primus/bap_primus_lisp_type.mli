open Core_kernel
open Bap.Std
open Bap_primus_lisp_types

module Value = Bap_primus_value

type t = typ [@@deriving compare, sexp]
val read : string -> typ option

module Check : sig
  val value : arch -> t -> Value.t -> bool
  val arg : arch -> t -> Arg.t -> bool
end

include Comparable.S_plain with type t := t
