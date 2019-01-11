(** Primus attributes.

    Attributes are declared with the [declare] statement. Each
    attribute has its own syntax. A parser can be registered using
    this module.

    So far, we keep this module internal.
*)

open Bap_lisp__types

type 'a t

type set

type error = ..
exception Unknown_attr of string * tree
exception Bad_syntax of error * tree list

(** registers a new attribute. An attribute is a monoind that is
    parsed from a sexp and added to existing attribute of the same
    kind.*)
val register :
  name:string ->
  add:('a -> 'a -> 'a) ->
  parse:(tree list -> 'a) -> 'a t

val parse : set -> tree -> set

module Set : sig
  val get : set -> 'a t -> 'a option
  val empty : set
end
