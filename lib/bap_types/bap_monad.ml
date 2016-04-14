open Core_kernel.Std
open Bap_monad_types
include Monad


module State = struct
  type ('a,'s) t = 's -> ('a * 's)
  type 'a result = 'a
  include Monad.Make2(struct
      type nonrec ('a,'s) t = ('a,'s) t
      let return x = (); fun s -> (x,s)
      let bind m f = (); fun s -> m s |> fun (x,s) -> f x s
      let map = `Define_using_bind
    end)
  let put s = (); fun _ -> ((),s)
  let get () = (); fun s -> (s,s)
  let state = ident

  let modify m f = (); fun s -> m s |> fun (x,s) -> x, f s
  let update f = get () >>= fun s -> put (f s)

  let gets f = get () >>| f

  let run m s = m s

  let eval m s = fst @@ run m s
  let exec m s = snd @@ run m s
end

module T = struct


  module Option = struct
    module Make(M : S) = struct
      type 'a t = 'a option M.t
      include Monad.Make(struct
          type nonrec 'a t = 'a t
          let return x : 'a t = Option.return x |> M.return
          let bind m f : 'b t  = M.bind m (function
              | Some r -> f r
              | None -> M.return None)
          let map = `Define_using_bind
        end)
      let lift (m : 'a option) : 'a t = M.return m
    end
    module Make2(M : Monad.S2) = struct
      type ('a,'b) t = ('a option,'b) M.t
      include Monad.Make2(struct
          type nonrec ('a,'b) t = ('a,'b) t
          let return x  = Option.return x |> M.return
          let bind m f   = M.bind m (function
              | Some r -> f r
              | None -> M.return None)
          let map = `Define_using_bind
        end)
      let lift m   = M.return m
    end
  end


  module Or_error = struct
    module Make(M : Monad.S) = struct
      type 'a t = 'a Or_error.t M.t
      include Monad.Make(struct
          type nonrec 'a t = 'a t
          let return x = Or_error.return x |> M.return
          let bind m f = M.bind m (function
              | Ok r -> f r
              | Error err -> M.return (Error err))
          let map = `Define_using_bind
        end)
      let lift m = M.return m
    end
    module Make2(M : Monad.S2) = struct
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
    module Make(M : Monad.S) = struct
      type ('a,'e) t = ('a,'e) Result.t M.t
      include Monad.Make2(struct
          type nonrec ('a,'e) t = ('a,'e) t
          let return x = Result.return x |> M.return
          let bind m f = M.bind m (function
              | Ok r -> f r
              | Error err -> M.return (Error err))
          let map = `Define_using_bind
        end)
      let lift m : ('a,'e) t = M.return m
    end
  end

  module State = struct
    module Make(M : Monad.S) = struct
      type ('a,'s) t = 's -> ('a * 's) M.t
      type 'a result = 'a M.t
      include Monad.Make2(struct
          type nonrec ('a,'s) t = ('a,'s) t
          let return x s = M.return (x,s)
          let bind m f s = M.bind (m s) (fun (x,s) -> f x s)
          let map = `Define_using_bind
        end)

      let mlift1 f x = M.(x >>= fun x -> return (f x))

      let get () = (); fun s -> M.return (s,s)
      let put s = (); fun _ -> M.return ((),s)
      let modify : ('a,'s) t -> ('s -> 's) -> ('a,'s) t =
        fun m f ->  (); fun s -> M.bind (m s) (fun (x,s) -> M.return (x, f s))
      let update f = get () >>= fun s -> put (f s)
      let gets f = get () >>| f
      let run m s = m s
      let eval m s = mlift1 fst (run m s)
      let exec m s = mlift1 snd (run m s)
    end
  end
end
