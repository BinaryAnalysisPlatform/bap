open Bap_knowledge

type 'a spec
type data
type ctrl
type full
type 'a t = ('a spec -> unit) Knowledge.Class.t

val data : data
val ctrl : ctrl
val full : full

val define : string -> 'a -> 'a t
val refine : string -> 'a t -> 'a t

val unknown : 'a t

val add : 'a t -> 'a t -> 'a t
val (+) : 'a t -> 'a t -> 'a t
val sum : 'a t list -> 'a t
val join : data t list -> ctrl t list -> full t

val order : 'a t -> 'a t -> Knowledge.Order.partial


val rreg : data t
val wreg : data t
val rmem : data t
val wmem : data t
val barr : data t


val fall : ctrl t
val jump : ctrl t
val cjmp : ctrl t
