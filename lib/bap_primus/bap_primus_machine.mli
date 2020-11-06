open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus_types

module type Component = Component
module type S = Machine

type id = Monad.State.Multi.id
type nonrec component = component

module State = Bap_primus_state
module Make(M : Monad.S) : S with type 'a m := 'a M.t


val exn_raised : exn observation
val kill : id observation
val start : string observation
val stop : string observation
val fork : (id * id) observation
val switch : (id * id) observation
