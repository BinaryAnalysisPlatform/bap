open Core_kernel
open Bap.Std
open Bap_primus_lisp_types

module Value = Bap_primus_value
module Context = Bap_primus_lisp_context

type t = typ [@@deriving compare, sexp]
type context = Context.t
type signature = {
  args : typ list;
  rest : typ option;
  ret  : typ;
}

val read : string -> typ option
val word : int -> t
val any : t
val var : string -> t

val signature : ?rest:t -> t list -> t -> signature


val pp : Format.formatter -> t -> unit

module Check : sig
  val value : t -> Value.t -> bool
  val arg : t -> Arg.t -> bool
end


include Comparable.S_plain with type t := t
