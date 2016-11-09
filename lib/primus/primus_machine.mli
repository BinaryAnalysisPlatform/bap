open Monads.Std
open Primus_types


module type Component = Component
module type S = Machine

type nonrec component = component

module Make(M : Monad.S) : Machine

module Main(M : Machine) : sig
  val run : ('a,#Context.t as 'e) M.t -> 'e -> (('a,error) result * 'e) M.m
end


val add_component : component -> unit
