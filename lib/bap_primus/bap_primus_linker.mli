open Bap.Std
open Regular.Std
open Bap_primus_types


type name = [
  | `tid of tid
  | `addr of addr
  | `symbol of string
] [@@deriving bin_io, compare, sexp]

type exn += Unbound_name of name

val exec : name observation
val will_exec : name statement

module type Code = functor (Machine : Machine) -> sig
  val exec : unit Machine.t
end

type code = (module Code)

module Name : Regular.S with type t = name

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
