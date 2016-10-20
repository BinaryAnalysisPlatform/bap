open Monads.Std
open Bap_bili_types

class context : Context.t
module type S = Bili.S
module Make( M : Monad.State.S2) : S with type ('a,'e) state = ('a,'e) M.t
include S with type ('a,'e) state = ('a,'e) Monad.State.t
