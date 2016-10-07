open Core_kernel.Std
open Bap_monad_types
include Monad


module StateT(M : Monad.S) = struct
  type ('a,'b) storage = {
    x : 'a;
    s : 'b;
  }
  type ('a,'s) t = {run : 's -> ('a, 's) storage M.t }
  type 'a result = 'a M.t
  include Monad.Make2(struct
    type nonrec ('a,'s) t = ('a,'s) t
    let return x = {run = fun s -> M.return {x;s}}
    let bind m f = {
      run = fun s -> M.bind (m.run s) (fun {x;s} ->
          (f x).run s)
    }
    let map = `Define_using_bind
  end)
  let put s    = {run = fun _ -> M.return {x=();s}}
  let get ()   = {run = fun s -> M.return {x=s;s}}
  let gets f   = {run = fun s -> M.return {x=f s;s}}
  let update f = {run = fun s -> M.return {x=();s = f s}}
  let modify (m : ('a,'s) t) f = {run = fun s ->
      M.bind (m.run s)
        (fun {x;s} -> M.return {x; s = f s})}
  let run m s = M.(m.run s >>| fun {x;s} -> (x,s))
  let eval m s = M.(run m s >>| fst)
  let exec m s = M.(run m s >>| snd)
  let lift m = {run = fun s ->
      M.bind m (fun x -> M.return {x;s})}
end

module Id = struct
  type 'a t = 'a
  include Monad.Make(struct
      type 'a t = 'a
      let return = ident
      let bind m f = m |> f
      let map = `Custom (fun x ~f -> f x)
    end)
end

module State = struct
  module type S = State
  include StateT(Id)
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
      let lift (m : 'a option) : ('a,'b) t = M.return m
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
    module Make = StateT
  end
end
