open Core_kernel.Std
open Bap.Std
open Monads.Std

open Bap_primus_generator_types

module Context = Bap_primus_context
class context = Context.t

type error =  Bap_primus_error.t = ..
type 'a observation = 'a Bap_primus_observation.t
type 'a statement = 'a Bap_primus_observation.statement
type 'a state = ('a,Context.t) Bap_primus_state.t
type ('a,'e) result = ('a,'e) Monad.Result.result =
  | Ok of 'a
  | Error of 'e

module type State = sig
  type ('a,'e) m
  type 'a t

  val get : 'a t -> ('a,#Context.t) m
  val put : 'a t -> 'a -> (unit,#Context.t) m
  val update : 'a t -> f:('a -> 'a) -> (unit,#Context.t) m
end

type id = Monad.State.Multi.id

module type Machine = sig
  type ('a,'e) t
  type 'a m

  module Observation : sig
    val observe : 'a observation -> ('a -> (unit,'e) t) -> (unit,'e) t
    val make : 'a statement -> 'a -> (unit,'e) t
  end

  module Syntax : sig
    include Monad.Syntax.S2 with type ('a,'e) t := ('a,'e) t
    val (>>>) : 'a observation -> ('a -> (unit,'e) t) -> (unit,'e) t
  end

  include Monad.State.Multi.S2 with type ('a,'e) t := ('a,'e) t
                                and type 'a m := 'a m
                                and type ('a,'e) e = 'e -> (('a, error) result * 'e) m
                                and type id := id
                                and module Syntax := Syntax
  module Local  : State with type ('a,'e) m := ('a,'e) t
                         and type 'a t := 'a state
  module Global : State with type ('a,'e) m := ('a,'e) t
                         and type 'a t := 'a state

  include Monad.Fail.S2 with type ('a,'e) t := ('a,'e) t
                         and type 'a error = error
end

module type Component = functor (Machine : Machine) -> sig
    val init : unit -> (unit,#Context.t) Machine.t
end

type component = (module Component)
