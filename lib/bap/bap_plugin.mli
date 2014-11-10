open Core_kernel.Std

type t


val load : t -> unit Or_error.t

val list : system:string -> t list

val name : t -> string

val path : t -> string

val system : t -> string

(** [load ~system] loads all finlib packages with entry
    [plugin_system] equal to [system], and returns a list of results
    of each load operation  *)
val load_all: system:string -> (t * unit Or_error.t) list
