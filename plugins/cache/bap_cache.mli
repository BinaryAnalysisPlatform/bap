open Bap_cache_types

val upgrade : unit -> unit

val size : unit -> int64

val mtime : unit -> float

module GC : sig
  val remove : int64 -> unit
  val remove_all : unit -> unit
end
