open Bap.Std
open Bap_primus_types

module Generator = Bap_primus_generator

type exn += Undefined_var of var

val generated : (var * value) Bap_primus_observation.t

module Make(Machine : Machine) : sig
  val get : var -> value Machine.t
  val set : var -> value -> unit Machine.t
  val add : var -> Generator.t -> unit Machine.t
  val del : var -> unit Machine.t
  val has : var -> bool Machine.t
  val all : var seq Machine.t
  val is_set : var -> bool Machine.t
end
