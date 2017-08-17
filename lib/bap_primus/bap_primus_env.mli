open Bap.Std
open Bap_primus_types

module Generator = Bap_primus_generator

type exn += Undefined_var of var

module Make(Machine : Machine) : sig
  val get : var -> value Machine.t
  val set : var -> value -> unit Machine.t
  val add : var -> Generator.t -> unit Machine.t
end
