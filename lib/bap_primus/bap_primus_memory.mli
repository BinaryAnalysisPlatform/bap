open Bap.Std
open Bap_primus_types

module Generator = Bap_primus_generator

type exn += Segmentation_fault of addr

val segmentation_fault : addr observation
val address_access : addr observation
val address_read : (addr * word) observation
val address_written : (addr * word) observation


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

  val is_mapped : addr -> bool Machine.t

  val is_writable : addr -> bool Machine.t
end
