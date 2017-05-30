open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus_types

module Observation = Bap_primus_observation

let components : component list ref = ref []
let add_component comp = components := comp :: !components

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
    curr    : unit t;
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
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      Observation.with_observers os key ~f:(List.iter ~f:(fun observe -> observe obs))

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

  let switch_task f =
    C.call ~cc:(fun k ->
        lifts (SM.update (fun s -> {s with curr = k (Ok ())})) >>= fun () ->
        lifts (f ()) >>= fun () ->
        lifts (SM.get ()) >>= fun s ->
        s.curr)

  let fork () : unit c = switch_task SM.fork
  let switch id : unit c = switch_task (fun () -> SM.switch id)

  let lift x = lifts (SM.lift x)
  let status x = lifts (SM.status x)
  let forks () = lifts (SM.forks ())
  let kill id = lifts (SM.kill id)
  let ancestor x  = lifts (SM.ancestor x)
  let parent () = lifts (SM.parent ())
  let global = SM.global
  let current () = lifts (SM.current ())
  let raise = fail
  let catch = catch

  let project = get ()
  let program = project >>| Project.program
  let arch = project >>| Project.arch
  let args = lifts (SM.gets @@ fun s -> s.args)
  let envp = lifts (SM.gets @@ fun s -> s.envp)

  let run : 'a t -> 'a e =
    fun m proj args envp ->
      M.bind
        (SM.run
           (C.run m (fun _ -> SM.gets @@ fun s -> (Ok s.proj))) {
           args;
           envp;
           curr = CM.return ();
           global = State.Bag.empty;
           local = State.Bag.empty;
           observations = Bap_primus_observation.empty;
           proj})
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

let finished,finish =
  Observation.provide ~inspect:sexp_of_unit "machine-finished"


module Main(Machine : Machine) = struct
  open Machine.Syntax

  let init_components () =
    Machine.List.iter !components ~f:(fun (module Component) ->
        let module Comp = Component(Machine) in
        Comp.init ())

  let run ?(envp=[| |]) ?(args=[| |]) proj m =
    let comp =
      init_components () >>= fun () -> m >>= fun x ->
      Machine.Observation.make finish () >>= fun () ->
      Machine.return x in
    Machine.run comp proj args envp
end


