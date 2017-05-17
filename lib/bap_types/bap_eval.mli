open Monads.Std
open Bap_eval_types

module type S = Eval.S
module Make(M : Monad.State.S2) : S with type ('a,'e) state = ('a,'e) M.t
include S with type ('a,'e) state = ('a,'e) Monad.State.t
