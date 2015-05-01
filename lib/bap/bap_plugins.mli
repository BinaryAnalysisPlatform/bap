(** Loads all known to bap plugins.

    If you want to load a plugin unknown to bap, use [Plugin] module
    directly.
*)

open Core_kernel.Std


module Std : sig
  type plugin

  module Plugin : sig
    type t = plugin

    (** [create ~system path] creates a plugin.

        Parameter [system] denotes the name of the extension point where
        the plugin would be plugged in. Path is should point to the plugin
        file. Plugin name is set to the [basename path].
    *)
    val create : system:string -> string -> t

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
    val load : unit -> unit
    val all : unit -> plugin list
  end

end
