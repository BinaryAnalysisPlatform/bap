open Core_kernel.Std
open Monads.Std

open Bap_knowledge
open Bap_core_theory

open Bap_primus_generator_types

module Exn = Bap_primus_exn

module Word = Bap.Std.Word
type word = Bap.Std.word [@@deriving bin_io, compare, sexp]
type addr = word [@@deriving bin_io, compare, sexp]

type knowledge = state
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


type 'a service = {
  init : 'a;
  name : string;
  desc : string;
}

module type Machine = sig
  type 'a t
  type 'a m

  val collect : 'a content -> label -> 'a t
  val provide : 'a content -> label -> 'a -> unit t
  val conflict : conflict -> 'a t
  val knowledge : 'a Knowledge.t -> 'a t

  val raise : exn -> 'a t
  val catch : 'a t -> (exn -> 'a t) -> 'a t

  val die : id -> unit t

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
                               and type id := id
                               and module Syntax := Syntax
                               and type 'a e =
                                     'a t service list ->
                                     knowledge ->
                                     (exit_status * knowledge, conflict) result

  module Local  : State with type 'a m := 'a t
                         and type 'a t := 'a state
  module Global : State with type 'a m := 'a t
                         and type 'a t := 'a state

end
