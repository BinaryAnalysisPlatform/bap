open Bap.Std
open Primus_types

module Generator = Primus_generator


module Make(Machine : Machine) : sig
  type ('a,'e) m = ('a,'e) Machine.t
  val get : var -> (Bil.result,#Context.t) m
  val set : var -> word -> (unit,#Context.t) m
  val add : var -> Generator.t -> (unit,#Context.t) m
end
