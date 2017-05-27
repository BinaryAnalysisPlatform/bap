open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus_types


module type Component = Component
module type S = Machine

type nonrec component = component

val finished : unit observation

module State = Bap_primus_state
module Make(M : Monad.S) : Machine with type 'a m := 'a M.t

type id = Monad.State.Multi.id

module Main(M : Machine) : sig
  val run : unit M.t -> project -> (project,error) result M.m
end


val add_component : component -> unit
