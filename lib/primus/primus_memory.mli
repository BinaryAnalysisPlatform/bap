open Bap.Std
open Primus_types

module Context = Primus_context

type error += Segmentation_fault of addr
type error += Stack_overflow of addr


val segmentation_fault : addr observation

module type S = sig
  type t
  type ('a,'e) m

  module Generator : Primus_generator.S with type ('a,'e) m := ('a,'e) m

  val load : addr -> (word,#Context.t) m
  val save : addr -> word -> (unit,#Context.t) m

  val add_text : mem -> (unit,#Context.t) m
  val add_data : mem -> (unit,#Context.t) m
  val allocate :
    ?readonly:bool ->
    ?executable:bool ->
    ?policy:Generator.policy ->
    addr -> int -> (unit,#Context.t) m
end


module Make(Machine : Machine) : S with type ('a,'e) m := ('a,'e) Machine.t
