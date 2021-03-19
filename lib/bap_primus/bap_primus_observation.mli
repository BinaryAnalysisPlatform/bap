open Core_kernel
open Bap_core_theory
open Bap.Std
open Bap_future.Std
open Monads.Std

type 'a t
type 'a statement
type 'e observations
type subscription
type provider

module Info = Bap_primus_info


val provide : ?desc:string -> ?inspect:('a -> Sexp.t) -> ?package:string ->
  string -> 'a t * 'a statement

val name : 'a t -> string
val inspect : 'a t -> 'a -> Sexp.t
val of_statement : 'a statement -> 'a t


val add_observer : 'e observations -> 'a t -> ('a -> 'e) -> 'e observations * subscription
val add_watcher : 'e observations -> provider -> (Sexp.t -> 'e) ->
  'e observations * subscription

val cancel : subscription -> 'e observations -> 'e observations

module Make(Machine : Monad.S) : sig
  val notify :
    unit Machine.t observations ->
    'a statement -> 'a -> unit Machine.t

  val notify_if_observed :
    unit Machine.t observations ->
    'a statement ->
    (('a -> unit Machine.t) -> unit Machine.t) -> unit Machine.t
end

val empty : 'e observations

val list_providers : unit -> provider list
val list : unit -> Info.t list

module Provider : sig
  type t = provider
  val name : t -> string
  val fullname : t -> KB.Name.t
  val observers : t -> int
  val triggers : t -> unit stream
  val data : t -> Sexp.t stream
end
