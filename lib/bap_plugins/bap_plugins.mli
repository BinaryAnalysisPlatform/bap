(** Bap Plugin Library. *)

open Core_kernel.Std
open Bap_bundle.Std
open Bap_future.Std


(** Interface to the plugin subsystem.  *)
module Std : sig


  type plugin


  (** Plugin - a loadable self-contained piece of code.

      Plugin is a bundle, that contains code, data and meta
      information.

  *)
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

    (** [load plugin] load all unsatisfied dependencies of a [plugin],
        and then load the [plugin] itself. If it is already
        loaded, then do nothing.

        Returns an error if some dependency can't be satisfied, or if
        there is a name conflict with already loaded or linked
        compilation unit. Can return error if the underlying loader
        raised an error, or package is malformed, or if there is some
        other system error. *)
    val load : t -> unit Or_error.t

    (** loaded event happens when a pass is succesfully loaded  *)
    val loaded : t -> unit future
  end


  (** Plugin loader.

      This module handles the set of plugins, visible to the
      platform.

      Plugins are looked at the following locations:
      - paths explicitly specified by a user (via [library]
        parameter);
      - paths specified by the [BAP_PLUGIN_PATH] environment
        variable (same syntax as PATH variable);
      - $prefix/lib/bap, where $prefix is the installation prefix,
        specified during the installation.

      The latter destination is used by the [bapbundle install]
      command, so this is the default repository of plugins.

      The plugin loader is an event driven system. Once, [Plugins.run]
      function is called, events of type [Plugins.events] will start
      to fire. By default they are intercepted by a logger, and by the
      default handler (see {!Plugins.run} function description about
      the default handler). The logger will write the infromation
      about events, that is useful for debugging issues with plugins.
  *)
  module Plugins : sig
    type event = [
      | `Opening of string (** a bundle with a plugin is opened     *)
      | `Loading of plugin (** a plugin loading process is started  *)
      | `Linking of string (** a library or module is linked in      *)
      | `Loaded  of plugin (** a plugin was successfully loaded     *)
      | `Errored of string * Error.t (** failed to load a plugin    *)
    ] [@@deriving sexp_of]

    (** [list ?provides ?library ()] scans a list of directories, provided by a
        [library] parameter, and, finally, [Config.libdir] for files with
        a [plugin] extension. Returns all plugins that provide a superset of [features]. *)
    val list : ?provides:string list -> ?library:string list -> unit -> plugin list

    (** [run ?provides ?library ?exclude] load and execute all plugins that can
        be found in the [library] paths, and are not in the [exclude]
        list. The default event handler will abort the program if
        there is any [Errored] event occurs, a message will be printed
        to the standard output. If [don't_setup_handlers] option is
        chosen, then events are not treated, so that a host program may
        setup a more fine granular error handling. *)
    val run :
      ?provides:string list ->
      ?don't_setup_handlers:bool ->
      ?library:string list ->
      ?exclude:string list -> unit -> unit

    (** [load ?provides ?library ?exclude] is like [run], but will not setup
        handlers, and will return a result of loading. Each element of
        the list is either an [Ok plugin] if a [plugin] was
        successfully loaded, or [Error (name,error)] if a plugin with
        the given [name] failed with [error].
    *)
    val load :
      ?provides:string list ->
      ?library:string list ->
      ?exclude:string list -> unit ->
      (plugin, string * Error.t) Result.t list


    (** plugin subsystem event stream.   *)
    val events : event stream

    (** [loaded] occurs when all plugins are loaded  *)
    val loaded : unit future
  end


  (** [list_loaded_units ()] evaluates to a list of compilation unit
      names currently loaded into the system. The list includes both,
      units loaded dynamically, and those that were statically linked
      into the host program.  *)
  val list_loaded_units : unit -> string list


  (** [setup_dynamic_loader loader] installs [loader] program. The
      [loader] takes a filename and loads it.
      This function is used internally, for different modes of
      execution (native, byte, topleve) *)
  val setup_dynamic_loader : (string -> unit) -> unit
end
