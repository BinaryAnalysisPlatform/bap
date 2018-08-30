open Bap.Std
open Bap_primus_types

module Generator = Bap_primus_generator

type exn += Pagefault of addr

module Make(Machine : Machine) : sig

  val load  : addr -> word Machine.t
  val store : addr -> word -> unit Machine.t

  val get : addr -> value Machine.t
  val set : addr -> value -> unit Machine.t

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
