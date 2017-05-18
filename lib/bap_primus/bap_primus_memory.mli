open Bap.Std
open Bap_primus_types

module Context = Bap_primus_context
module Generator = Bap_primus_generator

type error += Segmentation_fault of addr

val segmentation_fault : addr observation

module Make(Machine : Machine) : sig

  val load : addr -> word Machine.t
  val save : addr -> word -> unit Machine.t

  val add_text : mem -> unit Machine.t
  val add_data : mem -> unit Machine.t

  val allocate :
    ?readonly:bool ->
    ?executable:bool ->
    ?generator:Generator.t ->
    addr -> int -> unit Machine.t

  val map :
    ?readonly:bool ->
    ?executable:bool ->
    mem -> unit Machine.t
end
