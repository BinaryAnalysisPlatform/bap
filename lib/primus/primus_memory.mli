open Bap.Std
open Primus_types

module Context = Primus_context

type error += Segmentation_fault of addr
type error += Stack_overflow of addr


val segmentation_fault : addr observation


module type S = Memory
module Make(Machine : Machine) : S with type ('a,'e) m := ('a,'e) Machine.t
