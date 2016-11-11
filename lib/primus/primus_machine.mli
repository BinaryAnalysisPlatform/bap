open Core_kernel.Std
open Monads.Std
open Primus_types


module type Component = Component
module type S = Machine

type nonrec component = component


module State = Primus_state
module Make(M : Monad.S) : Machine with type 'a m = 'a M.t

(* think about: add more stuff  to the main? Like preinstantiated  *)
module Main(M : Machine) : sig
  val run : ('a,#Context.t as 'e) M.t -> 'e -> (('a,error) result * 'e) M.m
end


val add_component : component -> unit
