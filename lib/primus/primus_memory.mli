open Bap.Std
open Primus_types

module Context = Primus_context
module Generator = Primus_generator

type error += Segmentation_fault of addr
type error += Stack_overflow of addr

val segmentation_fault : addr observation

module Make(Machine : Machine) : sig
  type ('a,'e) m = ('a,'e) Machine.t

  val load : addr -> (word,#Context.t) m
  val save : addr -> word -> (unit,#Context.t) m

  val add_text : mem -> (unit,#Context.t) m
  val add_data : mem -> (unit,#Context.t) m

  val allocate :
    ?readonly:bool ->
    ?executable:bool ->
    ?generator:Generator.t ->
    addr -> int -> (unit,#Context.t) m

  val map :
    ?readonly:bool ->
    ?executable:bool ->
    mem -> (unit,#Context.t) m
end
