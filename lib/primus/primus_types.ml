open Core_kernel.Std
open Bap.Std
open Monads.Std

open Primus_generator_types

type error =  Primus_error.t = ..
type 'a observation = 'a Primus_observation.t
type 'a statement = 'a Primus_observation.statement
type ('a,'e) result = ('a,'e) Monad.Result.result =
  | Ok of 'a
  | Error of 'e

module Context = Primus_context

module type State = sig
  type ('a,'e) m
  type 'a t
  val create : ?inspect:('a -> Sexp.t) -> name:string -> (Context.t -> 'a) -> 'a t

  val get : 'a t -> ('a,#Context.t) m
  val put : 'a t -> 'a -> (unit,#Context.t) m
  val update : 'a t -> f:('a -> 'a) -> (unit,#Context.t) m
end


module type Generator = sig
  type t
  type ('a,'e) m
  type policy = [`random of t option | `static of word]
                [@@deriving sexp_of]

  val lcg : int -> t

  val byte : int -> t



  module Seeded : sig
    val lcg : unit -> (t,'e) m
    val byte : unit -> (t,'e) m
  end

  val create :
    (module Iterator.Infinite
      with type t = 'a
       and type dom = int) -> 'a -> t

  val with_init :
    (module Iterator.Infinite
      with type t = 'a
       and type dom = int) -> (Context.t -> 'a) -> t


  val next : t -> (int,#Context.t) m
end

module type Memory = sig
  type t
  type ('a,'e) m

  module Generator : Generator with type ('a,'e) m := ('a,'e) m

  val load : addr -> (word,#Context.t) m
  val save : addr -> word -> (unit,#Context.t) m

  val add_text : mem -> (unit,#Context.t) m
  val add_data : mem -> (unit,#Context.t) m
  val allocate :
    ?readonly:bool ->
    ?executable:bool ->
    ?policy:Generator.policy ->
    addr -> int -> (unit,#Context.t) m
end


module type Env = sig
  type ('a,'e) m

  module Generator : Generator with type ('a,'e) m := ('a,'e) m
  val get : var -> (word,#Context.t) m
  val set : var -> word -> (unit,#Context.t) m
  val add : var -> Generator.policy -> (unit,#Context.t) m
end



module type Machine = sig
  type ('a,'e) t
  type 'a m

  module Observation : sig
    val observe : 'a observation -> ('a -> (unit,'e) t) -> (unit,'e) t
    val make : 'a statement -> 'a -> (unit,'e) t
  end
  include Monad.State.Multi.S2 with type ('a,'e) t := ('a,'e) t
                                and type 'a m := 'a m
                                and type ('a,'e) e = 'e -> (('a, error) result * 'e) m
  module Local  : State with type ('a,'e) m := ('a,'e) t
  module Global : State with type ('a,'e) m := ('a,'e) t

  include Monad.Fail.S2 with type ('a,'e) t := ('a,'e) t
                         and type 'a error = error
end



module type Component = sig
  module Make(Machine : Machine) : sig
    val init : unit -> (unit,#Context.t) Machine.t
  end
end

type component = (module Component)
