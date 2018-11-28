open Bap_knowledge
open Bap_core_theory_sort
module Effect = Bap_core_theory_effect

type 'a t

val empty : 'a Effect.t -> 'a t
val get : 'b domain -> 'a t -> 'b
val put : 'b domain -> 'a t -> 'b -> 'a t
val kind : 'a t -> 'a Effect.t
val partial : 'a t -> 'a t -> Domain.Order.partial
val merge : 'a t -> 'a t -> 'a t
val semantics : 'a t -> semantics
