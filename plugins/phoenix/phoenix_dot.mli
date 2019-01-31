open Core_kernel
open Bap.Std

module Make(Env : sig
    val project : project
    val options : Phoenix_options.t
    module Target : Target
  end) : sig
  val fprint_graph : Format.formatter -> Symtab.fn -> unit
  val output_graph : Out_channel.t -> Symtab.fn -> unit
end
