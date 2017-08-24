open Core_kernel.Std
open Monads.Std
open Bap_common
open Bap_bil
open Bap_result
open Bap_type_error
open Bap_expi_types

class context : Context.t

module type S = Expi.S
module Make(M : Monad.State.S2) : S with type ('a,'e) state = ('a,'e) M.t

include S with type ('a,'e) state = ('a,'e) Monad.State.t

val eval : exp -> value
