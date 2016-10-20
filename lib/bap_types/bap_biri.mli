open Core_kernel.Std
open Monads.Std
open Bap_result
open Bap_ir
open Bap_biri_types

class context : ?main : sub term -> program term -> Context.t
module type S = Biri.S

module Make(M : Monad.State.S2) : S with type ('a,'e) state = ('a,'e) M.t
include S with type ('a,'e) state = ('a,'e) Monad.State.t
