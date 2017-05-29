open Bap.Std
open Bap_primus_types


type name = [
  | `tid of tid
  | `addr of addr
  | `symbol of string
] [@@deriving sexp_of]

type error += Unbound_name of name

module type Code = functor (Machine : Machine) -> sig
  val exec : unit Machine.t
end

type code = (module Code)

module Make(Machine : Machine) : sig
  type 'a m = 'a Machine.t

  val link :
    ?addr:addr ->
    ?name:string ->
    ?tid:tid ->
    code -> unit m

  val exec : name -> unit m

  val is_linked : name -> bool m

end
