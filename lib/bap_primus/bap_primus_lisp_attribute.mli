(** Primus attributes.

    Attributes are declared with the [declare] statement. Each
    attribute has its own syntax. A parser can be registered using
    this module.

    So far, we keep this module internal.
  *)

open Bap_primus_lisp_types

type 'a t

type set

exception Unknown_attr of (string * sexp)
exception Bad_syntax of sexp

(** registers a new attribute. An attribute is a monoind that is
    parsed from a sexp and added to existing attribute of the same
    kind.*)
val register : name:string -> add:('a -> 'a -> 'a) ->
  sexp_of:('a -> sexp) ->
  of_sexp:(sexp list -> 'a) -> 'a t


module Set : sig
  val get : set -> 'a t -> 'a option
  val empty : set
end
