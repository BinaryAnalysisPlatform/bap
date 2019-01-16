open Core_kernel
open Monads.Std
open Bap_primus_types

module type S = Machine

type id = Monad.State.Multi.id
val exn_raised : exn observation

module State = Bap_primus_state
include S
type 'a machine = 'a t

module Component : sig
  type t = unit machine service

  val name : t -> string
  val desc : t -> string

  val provide : ?desc:string -> name:string -> unit machine -> unit
  val list : unit -> t list
end

(** [init] event occurs just after all components have been
      initialized, and before the execution starts*)
val init : unit observation

(** The [finished] event occurs when the machine terminates.   *)
val finished : unit observation
