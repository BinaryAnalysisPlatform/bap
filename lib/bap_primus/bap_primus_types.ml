open Core_kernel.Std
open Bap.Std
open Monads.Std

open Bap_primus_generator_types

module Context = Bap_primus_context

type error =  Bap_primus_error.t = ..
type 'a observation = 'a Bap_primus_observation.t
type 'a statement = 'a Bap_primus_observation.statement
type 'a state = 'a Bap_primus_state.t
type ('a,'e) result = ('a,'e) Monad.Result.result =
  | Ok of 'a
  | Error of 'e

type input = project
type effect = (project, error) result


module type State = sig
  type 'a m
  type 'a t

  val get : 'a t -> 'a m
  val put : 'a t -> 'a -> unit m
  val update : 'a t -> f:('a -> 'a) -> unit m
end

type id = Monad.State.Multi.id

module type Machine = sig
  type 'a t
  type 'a m

  module Observation : sig
    val observe : 'a observation -> ('a -> unit t) -> unit t
    val make : 'a statement -> 'a -> unit t
  end

  module Syntax : sig
    include Monad.Syntax.S with type 'a t := 'a t
    val (>>>) : 'a observation -> ('a -> unit t) -> unit t
  end

  include Monad.State.Multi.S with type 'a t := 'a t
                               and type 'a m := 'a m
                               and type env := project
                               and type 'a e = input -> effect m
                               and type id := id
                               and module Syntax := Syntax
  module Local  : State with type 'a m := 'a t
                         and type 'a t := 'a state
  module Global : State with type 'a m := 'a t
                         and type 'a t := 'a state

  include Monad.Fail.S with type 'a t := 'a t
                        and type 'a error = error
end

module type Component = functor (Machine : Machine) -> sig
  val init : unit -> unit Machine.t
end

type component = (module Component)
