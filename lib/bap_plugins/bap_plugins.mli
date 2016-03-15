(** Bap Plugin Library *)

open Core_kernel.Std
open Bap_bundle.Std

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

    (** [load plugin] loads given [plugin]  *)
    val load : t -> unit Or_error.t
  end

  module Plugins : sig
    (** [list ?library ()] scans a list of directories, provided by a
        [library] parameter, and, finally, [Config.libdir] for files with
        a [plugin] extension.
    *)
    val list : ?library:string list -> unit -> plugin list

    (** [load ?library ()] loads [all ?library ()] plugins, and
        returns a list of results. Each result is either an
        [Ok plugin], if [plugin] was loaded, or [Error (path,err)]
        if plugin from a [path] has failed with an error [err].*)
    val load : ?library:string list ->
      ?exclude:string list -> unit -> (plugin, string * Error.t) Result.t list
  end

  val list_loaded_units : unit -> string list

  val setup_dynamic_loader : (string -> unit) -> unit
end
