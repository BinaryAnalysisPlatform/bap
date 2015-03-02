(** Loads all known to bap plugins.

    If you want to load a plugin unknown to bap, use [Plugin] module
    directly.
*)


module Std : sig
  type plugin = Bap_plugin.t

  module Plugin : module type of Bap_plugin with type t = plugin

  module Plugins : sig
    val load : unit -> unit
    val all : unit -> Bap_plugin.t list
  end

end
