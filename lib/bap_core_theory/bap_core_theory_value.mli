open Bap_knowledge
open Bap_core_theory_sort

type 'a t                      (* semantic value *)

val empty : 'a sort -> 'a t
val get : 'b domain -> 'a t -> 'b
val put : 'b domain -> 'a t -> 'b -> 'a t
val sort : 'a t -> 'a sort
val partial : 'a t -> 'a t -> Domain.Order.partial
val merge : 'a t -> 'a t -> 'a t
val semantics : 'a t -> semantics
