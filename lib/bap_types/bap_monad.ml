open Core_kernel.Std
open  Bap_monad_types
include Monad


module State : State = struct
  type ('a,'s) t = 's -> ('a * 's)
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
