open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus_types
open Format

module Observation = Bap_primus_observation


module type Component = Component
module type S = Machine
type nonrec component = component


module State = Bap_primus_state
type id = Monad.State.Multi.id

module Make(M : Monad.S) = struct
  module PE = struct
    type t = (project, exn) Monad.Result.result
  end
  module SM = struct
    include Monad.State.Multi.T2(M)
    include Monad.State.Multi.Make2(M)
  end

  type 'a t  = (('a,exn) Monad.Result.result,PE.t sm) Monad.Cont.t
  and 'a sm  = ('a,state) SM.t
  and state = {
    args    : string array;
    envp    : string array;
    curr    : unit -> id t;
    proj    : project;
    local   : State.Bag.t;
    global  : State.Bag.t;
    observations : unit t Observation.observations;
  }

  type 'a c = 'a t
  type 'a m = 'a M.t
  type 'a e = (exit_status * project) m effect

  module C = Monad.Cont.Make(PE)(struct
      type 'a t = 'a sm
      include Monad.Make(struct
          type 'a t = 'a sm
          let return = SM.return
          let bind = SM.bind
          let map = `Custom SM.map
        end)
    end)

  module CM = Monad.Result.Make(Exn)(struct
      type 'a t = ('a, PE.t sm) Monad.Cont.t
      include C
    end)

  type _ error = exn
  open CM

  type id = Monad.State.Multi.id
  module Id = Monad.State.Multi.Id

  (* lifts state monad to the outer monad *)
  let lifts x = CM.lift (C.lift x)

  let with_global_context (f : (unit -> 'a t)) =
    lifts (SM.current ())       >>= fun id ->
    lifts (SM.switch SM.global) >>= fun () ->
    f ()                >>= fun r  ->
    lifts (SM.switch id)        >>| fun () ->
    r

  let get_local () : _ t = lifts (SM.gets @@ fun s -> s.local)
  let get_global () : _ t = with_global_context @@ fun () ->
    lifts (SM.gets @@ fun s -> s.global)

  let set_local local = lifts @@ SM.update @@ fun s ->
    {s with local}

  let set_global global = with_global_context @@ fun () ->
    lifts (SM.update @@ fun s -> {s with global})

  module Observation = struct
    type 'a m = 'a t
    type nonrec 'a observation = 'a observation
    type nonrec 'a statement = 'a statement

    let observations () = lifts (SM.gets @@ fun s -> s.observations)

    let set_observations observations = with_global_context @@ fun () ->
      lifts (SM.update @@ fun s -> {s with observations})

    let make key obs =
      (* let event = Observation.of_statement key in *)
      with_global_context observations >>= fun os ->
      Seq.all_ignore @@ Observation.notify os key obs

    let observe key observer =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      set_observations (Observation.add_observer os key observer)
  end

  module Make_state(S : sig
      val get : unit -> State.Bag.t t
      val set : State.Bag.t -> unit t
      val typ : string
    end) = struct
    type 'a m = 'a t
    let get state =
      S.get () >>= fun states ->
      State.Bag.with_state states state
        ~ready:return
        ~create:(fun make ->
            lifts (SM.get ()) >>= fun {proj} ->
            return (make proj))

    let put state x =
      S.get () >>= fun states ->
      S.set (State.Bag.set states state x)

    let update data ~f =
      get data >>= fun s -> put data (f s)
  end

  module Local = Make_state(struct
      let typ = "local"
      let get = get_local
      let set = set_local
    end)

  module Global = Make_state(struct
      let typ = "global"
      let get = get_global
      let set = set_global
    end)

  let put proj = lifts @@ SM.update @@ fun s -> {s with proj}
  let get () = lifts (SM.gets @@ fun s -> s.proj)
  let project : project t = get ()
  let gets f = get () >>| f
  let update f = get () >>= fun s -> put (f s)
  let modify m f = m >>= fun x -> update f >>= fun () -> return x


  let fork_state () = lifts (SM.fork ())
  let switch_state id = lifts (SM.switch id)
  let store_curr k id =
    lifts (SM.update (fun s -> {s with curr = fun () -> k (Ok id)}))

  (* switch_task SM.fork *)



  let lift x = lifts (SM.lift x)
  let status x = lifts (SM.status x)
  let forks () = lifts (SM.forks ())
  let kill id = lifts (SM.kill id)
  let ancestor x  = lifts (SM.ancestor x)
  let parent () = lifts (SM.parent ())
  let global = SM.global
  let current () = lifts (SM.current ())

  let fork () : unit c =
    current () >>= fun pid ->
    C.call ~cc:(fun k -> store_curr k pid >>= fork_state >>= current) >>=
    switch_state

  let switch id : unit c =
    current () >>= fun cid ->
    C.call ~cc:(fun k ->
        store_curr k cid >>= fun () ->
        switch_state id >>= fun () ->
        lifts (SM.get ()) >>= fun s ->
        s.curr ()) >>=
    switch_state

  let raise = fail
  let catch = catch

  let project = get ()
  let program = project >>| Project.program
  let arch = project >>| Project.arch
  let args = lifts (SM.gets @@ fun s -> s.args)
  let envp = lifts (SM.gets @@ fun s -> s.envp)


  let init proj args envp = {
    args;
    envp;
    curr = current;
    global = State.Bag.empty;
    local = State.Bag.empty;
    observations = Bap_primus_observation.empty;
    proj}

  let extract f =
    let open SM.Syntax in
    SM.switch SM.global >>= fun () ->
    SM.gets f

  let run : 'a t -> 'a e =
    fun m proj args envp ->
      M.bind
        (SM.run
           (C.run m (function
                | Ok _ -> extract @@ fun s -> Ok s.proj
                | Error err -> extract @@ fun s -> Error err))
           (init proj args envp))
        (fun (r,{proj}) -> match r with
           | Ok _ -> M.return (Normal, proj)
           | Error e -> M.return (Exn e, proj))


  module Syntax = struct
    include CM.Syntax
    let (>>>) = Observation.observe
  end

  include (CM : Monad.S with type 'a t := 'a t
                         and module Syntax := Syntax)
end
