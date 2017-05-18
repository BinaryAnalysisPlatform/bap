open Bap.Std
open Bap_primus_types

module Generator = Bap_primus_generator

type error += Undefined_var of var

val undefined_variable : var observation

module Make(Machine : Machine) : sig
  val get : var -> word Machine.t
  val set : var -> word -> unit Machine.t
  val add : var -> Generator.t -> unit Machine.t
end
