(** All operations in this module are atomic. *)

open Bap_cache_types

val init : unit -> unit

val size : unit -> int64

val read_config : unit -> config

val write_config : config -> unit

module GC : sig
  val shrink : ?threshold:int64 -> upto:int64 -> unit -> unit
  val clean : unit -> unit
end
