open Core_kernel

module Domain = Bap_knowledge_domain

type t
type semantics = t
type 'a domain

val declare : name:string ->
  (module Domain.S with type t = 'a) -> 'a domain

val empty : t
val get : 'a domain -> t -> 'a
val put : 'a domain -> t -> 'a -> t
val domain : 'a domain -> (module Domain.S with type t = 'a)

include Domain.S with type t := t

module Sorted(Sort : T1) : sig
  type 'a t                      (* semantic value *)

  val empty : 'a Sort.t -> 'a t
  val get : 'b domain -> 'a t -> 'b
  val put : 'b domain -> 'a t -> 'b -> 'a t
  val kind : 'a t -> 'a Sort.t
  val partial : 'a t -> 'a t -> Domain.Order.partial
  val merge : 'a t -> 'a t -> 'a t
  val semantics : 'a t -> semantics
end
