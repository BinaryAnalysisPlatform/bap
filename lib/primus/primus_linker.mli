open Bap.Std
open Primus_types


type name = [
  | `tid of tid
  | `addr of addr
  | `symbol of string
] [@@deriving sexp_of]

module type Code = functor (Machine : Machine) -> sig
  val exec : unit -> (unit,#Context.t) Machine.t
end

type code = (module Code)


module Make(Machine : Machine) : sig
  type ('a,'e) m = ('a,'e) Machine.t

  val link :
    ?addr:addr ->
    ?name:string ->
    code:code -> tid -> (unit,#Context.t) m

  val exec : name -> (unit,#Context.t) m

  val is_linked : name -> (bool,#Context.t) m

  val resolve : name -> (string option,#Context.t) m
end
