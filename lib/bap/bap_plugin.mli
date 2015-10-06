open Core_kernel.Std

type t


val create : ?library:string list -> ?path:string -> system:string -> string -> t option

(** [load plugin] loads given [plugin]  *)
val load : t -> unit Or_error.t

val name : t -> string

val path : t -> string

val system : t -> string

(** [find ~system] loads all finlib packages in the findlib path with
    META file containing entry [plugin_system] equal to [system], and
    returns a list of results of each load operation *)
val find_all: system:string -> t list
