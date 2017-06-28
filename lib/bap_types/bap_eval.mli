open Monads.Std
open Bap_eval_types

module type S = Eval.S
module type S2 = Eval.S2
module Make2(M : Monad.S2) : S2 with type ('a,'e) m := ('a,'e) M.t
                                 and module M := M
module Make(M : Monad.S) : S with type 'a m := 'a M.t
                              and module M := M
