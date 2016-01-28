open Core_kernel.Std
open Bap_monad_types

module type Basic = Monad.Basic
module type Basic2 = Monad.Basic2

module type Infix = Monad.Infix
module type Infix2 = Monad.Infix2

module type S = Monad.S
module type S2 = Monad.S2

module Make(M : Basic) : S with type 'a t := 'a M.t
module Make2(M : Basic2) : S2 with type ('a,'s) t := ('a,'s) M.t

module State : State
