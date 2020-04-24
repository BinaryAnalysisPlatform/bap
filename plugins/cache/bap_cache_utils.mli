
open Core_kernel
open Regular.Std

val from_file :
  (module Binable.S with type t = 't) -> string -> 't

val from_file': 'a Data.Read.t -> string -> 'a

(** [to_file ?temp_dir b file data] -
    provides an atomic writing of [data] into [file] using [Binable]
    interface. The atomic property is hold if the provided [temp_dir]
    and [file] share the same filesystem. If [temp_dir] is not
    provided then parent directory for [file] is used instead. *)
val to_file :
  ?temp_dir:string ->
  (module Binable.S with type t = 't) ->
  string -> 't -> unit

(** [to_file' ?temp_dir writer file data] same as [to_file] above, but
    writes using [Regular.Data.Write] interface *)
val to_file' :
  ?temp_dir:string -> 'a Data.Write.t -> string -> 'a -> unit
