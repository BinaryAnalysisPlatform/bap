open Bap_core_theory_sort

type 'a t

val create : 'a sort -> string -> 'a t
val name : 'a t -> string
val sort : 'a t -> 'a sort
