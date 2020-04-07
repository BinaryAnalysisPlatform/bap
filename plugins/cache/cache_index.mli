
open Cache_types

val set_cache_dir : string -> unit

val cache_dir : unit -> string

val index_file : unit -> string

val with_lock : f:(unit -> 'a) -> 'a

val read : unit -> index

val write : index -> unit

val upgrade : unit -> unit
