(** Primus attributes.

    Attributes are declared with the [declare] statement. Each
    attribute has its own syntax. A parser can be registered using
    this module.

    So far, we keep this module internal.
*)

open Bap_primus_lisp_types
open Bap_core_theory

type 'a t
type set

type error = ..
exception Unknown_attr of string * tree
exception Failure of error * tree list

val declare :
  ?desc:string ->
  ?package:string ->
  domain:'a KB.domain ->
  parse:(tree list -> 'a) ->
  string -> 'a t


val parse : set -> tree -> set

module Set : sig
  include KB.Value.S with type t := set
  val get : 'a t -> set -> 'a
  val slot : (Theory.program, set) KB.slot
end

module Parse : sig
  type nonrec tree = tree
  type nonrec error = error = ..
  type error += Expect_atom | Expect_list

  val atom : tree -> string option
  val list : tree -> tree list option
  val tree :
    atom:(string -> 'a) ->
    list:(tree list -> 'a) ->
    tree -> 'a
  val fail : error -> tree list -> _
end
