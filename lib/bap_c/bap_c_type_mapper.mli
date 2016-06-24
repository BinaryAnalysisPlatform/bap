open Core_kernel.Std
open Bap.Std
open Bap_c_type_mapper_intf


include S with type ('a,'e) m = 'a


module Search : sig
  include Monad.S2

  val finished : 'e -> ('a,'e) t
  val result : ('a,'e) t -> 'e option
end

module State : S with type ('a,'e) m = ('a,'e) Monad.State.t

module Finder : S with type ('a,'e) m = ('a,'e) Search.t



module Make( M : Monad.S2) : S with type ('a,'e) m = ('a,'e) M.t
