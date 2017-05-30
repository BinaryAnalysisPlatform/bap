open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus_types

module type Component = Component
module type S = Machine

type id = Monad.State.Multi.id
type nonrec component = component

module State = Bap_primus_state
module Make(M : Monad.S) : Machine with type 'a m := 'a M.t

module Main(M : Machine) : sig
  val run : 
    ?envp:string array ->
    ?args:string array -> 
    project ->
    unit M.t ->
    (exit_status * project) M.m
end

val finished : unit observation
val add_component : component -> unit
