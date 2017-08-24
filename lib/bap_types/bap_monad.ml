open Core_kernel.Std

module Legacy = struct
  include Bap_monad_types
  include Monad
end

open Monads.Std

module State = struct
  module type S = Legacy.State
  module Compat = struct
    type 'a result = 'a
    include Monad.State
    let modify m f =
      m >>= fun x ->
      update f >>= fun () ->
      return x
  end
  include (Compat : S with type ('a,'e) t = ('a,'e) Monad.State.t
                       and type 'a result = 'a)

end

module T = struct
  module Option = struct
    module Make(M : Monad.Core) = struct
      type 'a t = 'a option M.t
      include Monad.Option.Make(Monad.Core(M))
    end
    module Make2(M : Monad.Core2) = struct
      type ('a,'b) t = ('a option,'b) M.t
      include Monad.Option.Make2(Monad.Core2(M))
    end
  end

  module Or_error = struct
    module Make(M : Monad.Core) = struct
      type 'a t = 'a Or_error.t M.t
      include Monad.Result.Error.Make(Monad.Core(M))
    end
    module Make2(M : Monad.Core2) = struct
      type ('a,'b) t = ('a Or_error.t,'b) M.t
      include Monad.Make2(struct
          type nonrec ('a,'b) t = ('a,'b) t
          let return x = Or_error.return x |> M.return
          let bind m f = M.bind m (function
              | Ok r -> f r
              | Error err -> M.return (Error err))
          let map = `Define_using_bind
        end)
      let lift m = M.return m
    end
  end

  module Result = struct
    module Make(M : Monad.Core) = struct
      type ('a,'e) t = ('a,'e) Result.t M.t
      include Monad.Result.Make2(Monad.Core(M))
    end
  end

  module State = struct
    module Make(M : Monad.Core) = struct
      module M = Monad.Core(M)
      type 'a result = 'a M.t
      include Monad.State.T2(M)
      include Monad.State.Make2(M)
      let modify m f =
        m >>= fun x ->
        update f >>= fun () ->
        return x
      let eval m s = M.(run m s >>| fst)
      let exec m s = M.(run m s >>| snd)
    end
  end
end

include Legacy
