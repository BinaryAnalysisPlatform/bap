open Core_kernel

module Domain = Knowledge_domain

type t
type semantics = t
type ('a,'r) data

val declare : name:string ->
  (module Domain.S with type t = 'a) -> ('a,'r) data

val empty : t
val get : ('a,_) data -> t -> 'a
val put : ('a,_) data -> t -> 'a -> t

val promise : ('a,'r) data -> 'r -> unit
val promises : (_,'r) data -> 'r list
val domain : ('a,'r) data -> (module Domain.S with type t = 'a)

include Domain.S with type t := t

module Sorted(Sort : T1) : sig
  type 'a t                      (* semantic value *)

  val empty : 'a Sort.t -> 'a t
  val get : ('b,_) data -> 'a t -> 'b
  val put : ('b,_) data -> 'a t -> 'b -> 'a t
  val kind : 'a t -> 'a Sort.t
  val partial : 'a t -> 'a t -> Domain.Order.partial
  val merge : 'a t -> 'a t -> 'a t
  val semantics : 'a t -> semantics
end
