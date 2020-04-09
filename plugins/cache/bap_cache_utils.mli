open Core_kernel

val from_file : (module Binable.S with type t = 't) -> string -> 't

val to_file : (module Binable.S with type t = 't) -> string -> 't -> unit
