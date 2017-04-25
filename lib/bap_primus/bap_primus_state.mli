open Core_kernel.Std

type ('a,'c) t
type ('a,'c) state = ('a,'c) t

type void
type uuid = (void,void,void) format


val declare :
  ?inspect:('a -> Sexp.t) ->
  uuid:uuid ->
  name:string ->
  ('c -> 'a) -> ('a,'c) t

val inspect : ('a,'c) t -> 'a -> Sexp.t
val name : ('a,'c) t -> string

module Bag : sig
  type t

  val empty : t
  val with_state : t -> ('a,'c) state ->
    ready:('a -> 'b) ->
    create:(('c -> 'a) -> 'b) -> 'b

  val set : t -> ('a,'c) state -> 'a -> t
end
