open Core_kernel.Std
open Monads.Std

open Bap_knowledge
open Bap_core_theory

open Bap_primus_generator_types

module Exn = Bap_primus_exn

module Word = Bap.Std.Word
type word = Bap.Std.word [@@deriving bin_io, compare, sexp]
type addr = word [@@deriving bin_io, compare, sexp]

type env = state
type exn = Exn.t = ..
type 'a observation = 'a Bap_primus_observation.t
type provider = Bap_primus_observation.provider
type 'a statement = 'a Bap_primus_observation.statement
type 'a state = 'a Bap_primus_state.t
type exit_status =
  | Normal
  | Exn of exn

module type State = sig
  type 'a m
  type 'a t

  val get : 'a t -> 'a m
  val put : 'a t -> 'a -> unit m
  val update : 'a t -> f:('a -> 'a) -> unit m
end

type value = {
  id : Int63.t;
  value  : word;
} [@@deriving bin_io]

type id = Monad.State.Multi.id

module type Machine = sig
  type 'a t
  type 'a m

  module Observation : sig
    val observe : 'a observation -> ('a -> unit t) -> unit t
    val watch : provider -> (Sexp.t -> unit t) -> unit t
    val make : 'a statement -> 'a -> unit t
  end

  module Syntax : sig
    include Monad.Syntax.S with type 'a t := 'a t
    val (>>>) : 'a observation -> ('a -> unit t) -> unit t
  end

  include Monad.State.Multi.S with type 'a t := 'a t
                               and type 'a m := 'a m
                               and type env := env
                               and type id := id
                               and module Syntax := Syntax
                               and type 'a e = ('a * env, exn) result m
  module Local  : State with type 'a m := 'a t
                         and type 'a t := 'a state
  module Global : State with type 'a m := 'a t
                         and type 'a t := 'a state

  val raise : exn -> 'a t
  val catch : 'a t -> (exn -> 'a t) -> 'a t

  val args : string array t
  val envp : string array t
end

module type Component = functor (Machine : Machine) -> sig
  val init : unit -> unit Machine.t
end

type component = (module Component)
