open Core_kernel
open Bap_knowledge

type 'a t
type 'a state = 'a t

type void
type uuid = (void,void,void) format


val declare :
  ?inspect:('a -> Sexp.t) ->
  uuid:uuid ->
  name:string ->
  'a knowledge -> 'a t

val inspect : 'a t -> 'a -> Sexp.t
val name : 'a t -> string

module Bag : sig
  type t

  val empty : t
  val with_state : t -> 'a state ->
    ready:('a -> 'b) ->
    create:('a knowledge -> 'b) -> 'b

  val set : t -> 'a state -> 'a -> t
end
