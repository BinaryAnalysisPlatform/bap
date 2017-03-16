open Bap.Std
open Primus_types


type name = [
  | `tid of tid
  | `addr of addr
  | `symbol of string
] [@@deriving sexp_of]

module type Code = functor (Machine : Machine) -> sig
  val exec : (#Context.t as 'a) Biri.Make(Machine).t -> (unit,'a) Machine.t
end

type code = (module Code)


module Make(Machine : Machine) : sig
  type ('a,'e) m = ('a,'e) Machine.t
   module Biri : Biri.S
     with type ('a,'e) state = ('a,'e) Machine.t

  val link :
    ?addr:addr ->
    ?name:string ->
    ?tid:tid ->
    code -> (unit,#Context.t) m

  val exec : name -> (#Context.t as 'a) #Biri.t -> (unit,'a) m

  val is_linked : name -> (bool,#Context.t) m

end
