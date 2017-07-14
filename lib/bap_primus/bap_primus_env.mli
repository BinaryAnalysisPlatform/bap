open Bap.Std
open Bap_primus_types

module Generator = Bap_primus_generator

type exn += Undefined_var of var

val undefined_variable : var observation
val variable_access : var observation
val variable_read : (var * value) observation
val variable_written : (var * value) observation

module Make(Machine : Machine) : sig
  val get : var -> value Machine.t
  val set : var -> value -> unit Machine.t
  val add : var -> Generator.t -> unit Machine.t
end
