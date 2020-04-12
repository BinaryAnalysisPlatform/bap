open Bap_cache_types

val upgrade : unit -> unit

val size : unit -> int64

val mtime : unit -> float

module GC : sig

  val shrink : upto:int64 -> unit

  val shrink_by_threshold : config -> unit

  val clean : unit -> unit

end
