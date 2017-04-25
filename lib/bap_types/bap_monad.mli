[@@@deprecated "use new monads library instead"]

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

module State : sig
  module type S = State
  include State with type 'a result = 'a
                 and type ('a,'e) t = ('a,'e) Monads.Std.Monad.State.t
end

module T : sig
  module Option : sig
    module Make (M : S ) : S  with type 'a t = 'a option M.t
    module Make2(M : S2) : S2 with type ('a,'b) t = ('a option,'b) M.t
  end

  module Or_error : sig
    module Make (M : S ) : S  with type 'a t = 'a Or_error.t M.t
    module Make2(M : S2) : S2 with type ('a,'b) t = ('a Or_error.t,'b) M.t
  end

  module Result : sig
    module Make(M : S) : S2 with type ('a,'e) t = ('a,'e) Result.t M.t
  end

  module State : sig
    module Make(M : S) : State with type 'a result = 'a M.t
  end
end
