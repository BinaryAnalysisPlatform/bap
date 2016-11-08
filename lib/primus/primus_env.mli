open Bap.Std
open Primus_types

module Context = Primus_context

module type S = sig
  type ('a,'e) m

  module Generator : Primus_generator.S with type ('a,'e) m := ('a,'e) m
  val get : var -> (word,#Context.t) m
  val set : var -> word -> (unit,#Context.t) m
  val add : var -> Generator.policy -> (unit,#Context.t) m
end


module Make(Machine : Machine) : S with type ('a,'e) m := ('a,'e) Machine.t
