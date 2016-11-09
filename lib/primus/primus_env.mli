open Bap.Std
open Primus_types

module Context = Primus_context

module type S = Env

module Make(Machine : Machine) : S with type ('a,'e) m := ('a,'e) Machine.t
