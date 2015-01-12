open Core_kernel.Std
open Core_lwt_container_intf

module Lift_sequence(M : Monad)
  : Monad_sequence with type 'a monad := 'a M.t
                    and type 'a t = 'a Sequence.t

module Lift_list(M : Monad)
  : Monad_sequence with type 'a monad := 'a M.t
                    and type 'a t := 'a list


module Lift(M:Monad)(T : sig
                       type 'a t
                       val to_sequence : 'a t -> 'a Sequence.t
                       val of_sequence : 'a Sequence.t -> 'a t
                     end)
  : Monad_sequence with type 'a monad := 'a M.t
                    and type 'a t = 'a T.t
