open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_result
open Bap_type_error
open Bap_monad_types

open Bap_expi_types


class context : Context.t

module type S = Expi.S
module Make(M : State) : S with type ('a,'e) state = ('a,'e) M.t

include S with type ('a,'e) state = ('a,'e) Bap_monad.State.t
