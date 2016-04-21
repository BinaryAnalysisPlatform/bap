(** Bap Plugin Library *)

open Core_kernel.Std
open Bap_bundle.Std
open Bap_future.Std

module Std : sig
  type plugin

  module Plugin : sig
    type t = plugin

    (** [of_path path] creates a plugin of a give [path]  *)
    val of_path : string -> t

    (** [path plugin] is a plugin's path  *)
    val path : t -> string

    (** [name plugin] is a plugin's name  *)
    val name : t -> string

    (** [desc plugin] returns plugin description  *)
    val desc : t -> string

    (** [bundle plugin] returns a plugin's bundle *)
    val bundle : t -> bundle

    (** [load plugin] loads given [plugin]. If it is already
        loaded, then do nothing. *)
    val load : t -> unit Or_error.t

    (** loaded event happens when a pass is succesfully loaded  *)
    val loaded : t -> unit future
  end

  module Plugins : sig
    type event = [
      | `Opening of string (** a bundle with a plugin is opened     *)
      | `Loading of plugin (** a plugin loading process is started  *)
      | `Linking of string (** a library or module is linked in      *)
      | `Loaded  of plugin (** a plugin was successfully loaded     *)
      | `Errored of string * Error.t (** failed to load a plugin    *)
    ] [@@deriving sexp_of]

    (** [list ?library ()] scans a list of directories, provided by a
        [library] parameter, and, finally, [Config.libdir] for files with
        a [plugin] extension.*)
    val list : ?library:string list -> unit -> plugin list

    (** [run ?library ?exclude] load and execute all plugins that can
        be found in the [library] paths, and are not in the [exclude]
        list. The default event handler will abort the program if
        there is any [Errored] event occurs, a message will be printed
        to the standard output. If [don't_setup_handlers] option is
        chosen, then events are not treated, so that a host program may
        setup a more fine granular error handling. *)
    val run :
      ?don't_setup_handlers:bool ->
      ?library:string list ->
      ?exclude:string list -> unit -> unit

    (** [load ?library ?exclude] is like [run], but will not setup
        handlers, and will return a result of loading. Each element of
        the list is either an [Ok plugin] if a [plugin] was
        successfully loaded, or [Error (name,error)] if a plugin with
        the given [name] failed with [error].
    *)
    val load :
      ?library:string list ->
      ?exclude:string list -> unit ->
      (plugin, string * Error.t) Result.t list


    (** plugin subsystem event stream.   *)
    val events : event stream

    (** [loaded] occurs when all plugins are loaded  *)
    val loaded : unit future
  end

  val list_loaded_units : unit -> string list

  val setup_dynamic_loader : (string -> unit) -> unit
end
