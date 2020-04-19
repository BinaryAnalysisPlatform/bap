open Bap_cache_types

type t = config [@@deriving bin_io, compare, sexp]

val set_cache_dir : string -> unit

val cache_dir : unit -> string

val cache_data : unit -> string

val lock_file : unit -> string

val config_path : unit -> string

val default : config

val gc_threshold : config -> int64


val unsafe_read : unit -> config

val unsafe_write : config -> unit

val unsafe_init : unit -> unit
