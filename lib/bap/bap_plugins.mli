(** Loads all known to bap plugins.

    If you want to load a plugin unknown to bap, use [Plugin] module
    directly.
*)

val load : unit -> unit

val all : unit -> Bap_plugin.t list
