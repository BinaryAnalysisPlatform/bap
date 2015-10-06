(** Loads all known to bap plugins.

    If you want to load a plugin unknown to bap, use [Plugin] module
    directly.
*)

open Core_kernel.Std


module Std : sig
  type plugin

  module Plugin : sig
    type t = plugin

    (** [create ?library ?path ~system name] if file is not [None]
        then create a plugin targeting this file, otherwise look at
        current working directory for file named [name.plugin], if it
        doesn't exist, then search for the plugin with a given [name]
        and [system] first in folders specified by [BAP_PLUGIN_PATH]
        environment variable, and if nothing found, continue with all
        folders specified with [library] parameter, and finally search
        in findlib system.
        If more than one plugin exists, then the first found is used.
    *)
    val create :
      ?library:string list ->
      ?path:string ->
      system:string -> string -> t option

    (** [load plugin] loads given [plugin]  *)
    val load : t -> unit Or_error.t

    val name : t -> string

    val path : t -> string

    val system : t -> string

    (** [find ~system] loads all finlib packages in the findlib path with
        META file containing entry [plugin_system] equal to [system], and
        returns a list of results of each load operation *)
    val find_all: system:string -> t list

  end

  module Plugins : sig
    val load : ?systems:string list -> unit -> unit
    val all : unit -> plugin list
  end

end
