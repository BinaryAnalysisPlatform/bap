open Bap_cache_types

val upgrade : unit -> unit

val size : unit -> int64

module GC : sig
  val shrink : ?threshold:int64 -> upto:int64 -> unit -> unit
  val clean : unit -> unit
end
