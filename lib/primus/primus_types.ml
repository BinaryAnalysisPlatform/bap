open Core_kernel.Std
open Bap.Std
open Monads.Std

type error = Primus_error.t

module type State = sig
  type ('a,'e) m
  type 'a t
  val create : ?observe:('a -> Sexp.t) -> name:string -> (Context.t -> 'a) -> 'a t

  val get : 'a t -> ('a,#Context.t) m
  val put : 'a t -> 'a -> (unit,#Context.t) m
  val update : 'a t -> f:('a -> 'a) -> (unit,#Context.t) m
end

(* TODO: splt Machine into a Deterministic part, and a *)
(* non-deterministic.  *)
module type Deterministic = sig
end

module type Machine = sig
  type ('a,'e) t
  type 'a m

  module Local  : State with type ('a,'e) m := ('a,'e) t
  module Global : State with type ('a,'e) m := ('a,'e) t
  include Monad.Fail.S2 with type ('a,'e) t := ('a,'e) t
                         and type 'a error = error
  include Monad.State.Multi.S2 with type ('a,'e) t := ('a,'e) t
                                and type 'a m := 'a m


end
