open Core_kernel


val unsafe_from_file :
  (module Binable.S with type t = 't) -> string -> 't

val unsafe_to_file :
  (module Binable.S with type t = 't) -> string -> 't -> unit
