(** Bap Plugin Library. *)

open Core_kernel
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
    val load : ?argv:string array -> t -> unit Or_error.t

    (** loaded event happens when a pass is successfully loaded  *)
    val loaded : t -> unit future

    val argv : unit -> string array
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
      the default handler). The logger will write the information
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



    (** [path] is the default directory where plugins are stored.  *)
    val path : string

    (** [list ()] is a list of available plugins.

        Returns all plugins that provide features specified by
        [provides] and meet the constraint specified by [env]. If a
        non-valid plugin is found in the search path, then it is
        ignored.

        @param provides if specified then only those plugins that
        provide at least one feature from the specified list are
        selected. Otherwise, all plugins are selected.

        @param env is a set of features provided by the runtime
        (defaults to none). If specified, then all plugins that require
        a set of features that is a subset of [env] will be
        selected. Note, usually, plugins do not have any requirements
        (i.e., the have an empty set of requirements), therefore all
        are selected.

        @param library is the list of additional directories in which
        plugins are searched. If specified then it is prepended to the
        default search path that contains (in that order):
        - the list of paths specified by the BAP_PLUGIN_PATH
          environment variable;
        - the [Plugins.directory] constant.

         Note, any non-files or non-directories are silently excluded from
         the list.

    *)
    val list :
      ?env:string list ->
      ?provides:string list ->
      ?library:string list ->
      unit -> plugin list

    (** [run ()] loads all plugins.

        This command will load all plugins available in the system
        (see the {!load} and {!list} functions for more information on
        which plugins are available). If an error occurs during an
        attempt to load a plugin and if [don't_setup_handlers] option
        is not set to [true], an extended error message is printed to
        the standard error channel and the currently running program is
        terminated with exit code 1.

        This function calls the [load] function and then observes
        (unless [don't_setup_handlers] is set) events that occur
        during loading to intercept any errors. So see the {!load}
        function for more detailed information.

        @param don't_setup_handlers if set to [true] (defaults to
        [false]) then all erros are silently ignored. Don't use it
        unless you're implementing your own error handling facility
        via the event subsystem.*)
    val run :
      ?argv:string array ->
      ?env:string list ->
      ?provides:string list ->
      ?don't_setup_handlers:bool ->
      ?library:string list ->
      ?exclude:string list ->
      unit -> unit

    (** [load ()] loads all plugins if they are not loaded yet.

        Loads all plugins available in the system and returns a list
        of values in which each entry corresponds to a result of a
        loading attempt, which could be either a successfully loaded
        plugin or an error, in case if the attempt has failed.

        The list of all plugins is [list ?env ?provides ?library ()]
        which is furthermore refined by removing all plugins which
        names are specified in the [exclude] blacklist.

        This function returns silently if [Plugins.loaded] is already
        decided. No matter the outcome, the [Plugins.loaded] future
        will be decided.
    *)
    val load :
      ?argv:string array ->
      ?env:string list ->
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
