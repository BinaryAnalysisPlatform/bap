open Core_kernel.Std
open Bap.Std

module Make(Env : Printing.Env) : sig
  val fprint_graph : Format.formatter -> mem -> unit
  val output_graph : out_channel -> mem -> unit
end
