open Core_kernel
open Bap.Std
open Monads.Std

open Bap_primus_generator_types

module Exn = Bap_primus_exn
module Pos = Bap_primus_pos

type exn = Exn.t = ..
type pos = Pos.t [@@deriving sexp_of]
type 'a observation = 'a Bap_primus_observation.t
type provider = Bap_primus_observation.provider
type 'a statement = 'a Bap_primus_observation.statement
type subscription = Bap_primus_observation.subscription
type 'a state = 'a Bap_primus_state.t
type exit_status =
  | Normal
  | Exn of exn

type 'a effect =
  ?envp:string array ->
  ?args:string array ->
  string ->
  project -> 'a

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
    val subscribe : 'a observation -> ('a -> unit t) -> subscription t
    val cancel : subscription -> unit t
    val watch : provider -> (Sexp.t -> unit t) -> unit t
    val make : 'a statement -> 'a -> unit t
    val post : 'a statement -> f:(('a -> unit t) -> unit t) -> unit t
  end

  module Syntax : sig
    include Monad.Syntax.S with type 'a t := 'a t
    val (>>>) : 'a observation -> ('a -> unit t) -> unit t
  end

  include Monad.State.Multi.S with type 'a t := 'a t
                               and type 'a m := 'a m
                               and type env := project
                               and type id := id
                               and module Syntax := Syntax
                               and type 'a e =
                                     ?boot:unit t ->
                                     ?init:unit t ->
                                     (exit_status * project) m effect
  module Local  : State with type 'a m := 'a t
                         and type 'a t := 'a state
  module Global : State with type 'a m := 'a t
                         and type 'a t := 'a state

  val raise : exn -> 'a t
  val catch : 'a t -> (exn -> 'a t) -> 'a t

  val project : project t
  val program : program term t
  val arch : arch t
  val args : string array t
  val envp : string array t
end

module type Component = functor (Machine : Machine) -> sig
  val init : unit -> unit Machine.t
end

type component = (module Component)
