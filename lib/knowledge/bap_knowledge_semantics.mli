open Core_kernel

module Domain = Bap_knowledge_domain

type t [@@deriving bin_io, compare, sexp]
type semantics = t
type 'a domain

val declare :
  ?serializer:(module Binable.S with type t = 'a) ->
  name:string ->
  (module Domain.S with type t = 'a) -> 'a domain

val empty : t
val create : 'a domain -> 'a -> t
val get : 'a domain -> t -> 'a
val put : 'a domain -> t -> 'a -> t
val merge : t -> t -> t
val domain : 'a domain -> (module Domain.S with type t = 'a)

include Domain.S with type t := t

module Sorted(Sort : T1) : sig
  type 'a t                      (* semantic value *)

  val create : 'a Sort.t -> semantics -> 'a t
  val empty : 'a Sort.t -> 'a t
  val get : 'b domain -> 'a t -> 'b
  val put : 'b domain -> 'a t -> 'b -> 'a t
  val kind : 'a t -> 'a Sort.t
  val partial : 'a t -> 'a t -> Domain.Order.partial
  val merge : 'a t -> 'a t -> 'a t
  val semantics : 'a t -> semantics
end

val domains : unit -> string list
val pp : Format.formatter -> t -> unit
val pp_domains : string list -> Format.formatter -> t -> unit
