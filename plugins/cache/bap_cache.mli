(** All operations in this module are atomic. *)

open Bap_cache_types

val init : unit -> unit

val size : unit -> int64
