open Core_kernel.Std
open Bap.Std

module Make(Env : sig
    val project : project
    val options : Options.t
    module Target : Target
  end) : sig
  val fprint_graph : Format.formatter -> Symtab.fn -> unit
  val output_graph : out_channel -> Symtab.fn -> unit
end
