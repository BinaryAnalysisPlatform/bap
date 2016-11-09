open Primus_types

module type S = Linker
module Make(Machine : Machine) : S with type ('a,'e) m := ('a,'e) Machine.t
