open Core_kernel.Std
open Bap_result
open Bap_ir
open Bap_monad_types
open Bap_biri_types

class context : ?main : sub term -> program term -> Context.t
module type S = Biri.S

module Make(M : State) : S with type ('a,'e) state = ('a,'e) M.t
include S with type ('a,'e) state = ('a,'e) Bap_monad.State.t
