open Bap_knowledge
open Bap_core_theory_sort
module Kind = Bap_core_theory_kind

type 'a t

val create : 'a Kind.t -> semantics -> 'a t
val empty : 'a Kind.t -> 'a t
val get : 'b domain -> 'a t -> 'b
val put : 'b domain -> 'a t -> 'b -> 'a t
val kind : 'a t -> 'a Kind.t
val partial : 'a t -> 'a t -> Domain.Order.partial
val merge : 'a t -> 'a t -> 'a t
val semantics : 'a t -> semantics
