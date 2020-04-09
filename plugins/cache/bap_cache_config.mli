open Bap_cache_types


val set_cache_dir : string -> unit

val cache_dir : unit -> string

val config_path : unit -> string

val read : unit -> config

val write : config -> unit

val default : config
