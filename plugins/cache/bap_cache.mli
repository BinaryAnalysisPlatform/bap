(** All operations in this module are atomic. *)

open Bap_cache_types

val init : unit -> unit

val size : unit -> int

val set_root : string -> unit

val root : unit -> string

val data : unit -> string

val gc_threshold : config -> int

val read_config : unit -> config

val write_config : config -> unit
