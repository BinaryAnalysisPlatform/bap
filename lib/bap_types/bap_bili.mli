open Bap_monad_types
open Bap_bili_types

class context : Context.t
module type S = Bili.S
module Make( M : State) : S with type ('a,'e) state = ('a,'e) M.t
include S with type ('a,'e) state = ('a,'e) Bap_monad.State.t
