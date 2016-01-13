open Core_kernel.Std

type t


(** [of_path path] create a plugin of the provided path  *)
val of_path : string -> t

(** [find_plugin ?library name] searches for a plugin named
    [name.plugin] or [name] if [name] ends with [.plugin] in
    current directory, then in each directory specified by
    [BAP_PLUGIN_PATH] environment variable, then in each folder
    specified by a [library] list (defaults to an empty list).
    Returns the first found plugin, if any. *)
val find_plugin : ?library:string list -> string -> t option

(** [find_library] searches using findlib for a library that is
    dynamically linkable and has a [plugin_system] field set to
    "bap.plugin". *)
val find_library : string -> t option

(** [find_libraries ()] loads all finlib packages in the findlib path
    with META file containing entry [plugin_system] equal to [system],
    and returns a list of results of each load operation *)
val find_libraries: unit -> t list

(** [load plugin] loads given [plugin]  *)
val load : t -> unit Or_error.t

val path : t -> string

val name : t -> string
