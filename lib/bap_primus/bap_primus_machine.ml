open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus_types

module Error = Bap_primus_error
module Observation = Bap_primus_observation

let components : component list ref = ref []
let add_component comp = components := comp :: !components

module type Component = Component
module type S = Machine
type nonrec component = component


module State = Bap_primus_state
type id = Monad.State.Multi.id

module Make(M : Monad.S) = struct
  module SM = struct
    include Monad.State.Multi.T2(M)
    include Monad.State.Multi.Make2(M)
  end

  type 'a t = (('a,Error.t) result,state) SM.t
  and state = {
    proj : project;
    local  : State.Bag.t;
    global : State.Bag.t;
    states : int String.Map.t;
    observations : unit t Observation.observations;
  }

  type 'a m = 'a M.t
  type 'a e = project -> (('a,Error.t) result * project) m
  module Basic = struct
    open SM.Syntax
    type nonrec 'a t = 'a t
    let return x = SM.return (Ok x)

    let bind (m : 'a t) (f : 'a -> 'b t) : 'b t = m >>= function
      | Ok r -> f r
      | Error err -> SM.return (Error err)
    let map = `Define_using_bind
  end

  module Fail = struct
    let fail err = SM.return (Error err)
    let catch m f = SM.bind m (function
        | Error err -> f err
        | ok -> SM.return ok)
  end

  type _ error = Error.t
  include Fail
  module M2 = Monad.Make(Basic)
  open M2



  type id = Monad.State.Multi.id
  module Id = Monad.State.Multi.Id

  let lifts m = SM.map m ~f:(fun x -> Ok x)

  let with_global_context (f : (unit -> 'a t)) =
    lifts (SM.current ())       >>= fun id ->
    lifts (SM.switch SM.global) >>= fun () ->
    f ()                >>= fun r  ->
    lifts (SM.switch id)        >>| fun () ->
    r

  let get_local () = lifts (SM.gets @@ fun s -> s.local)
  let get_global () = with_global_context @@ fun () ->
    SM.gets @@ fun s -> Ok s.global

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

  let run : 'a t -> 'a e = fun m proj ->
    M.bind (SM.run m {
        global = State.Bag.empty;
        local = State.Bag.empty;
        observations = Bap_primus_observation.empty;
        states = String.Map.empty;
        proj}) @@ fun (x,{proj}) -> M.return (x,proj)

  let eval m s = M.map (run m s) ~f:fst
  let exec m s = M.map (run m s) ~f:snd
  let lift x = lifts (SM.lift x)
  let status x = lifts (SM.status x)
  let forks () = lifts (SM.forks ())
  let kill id = lifts (SM.kill id)
  let fork () = lifts (SM.fork ())
  let ancestor x  = lifts (SM.ancestor x)
  let parent () = lifts (SM.parent ())
  let switch id = lifts (SM.switch id)
  let global = SM.global
  let current () = lifts (SM.current ())

  module Syntax = struct
    include M2.Syntax
    let (>>>) = Observation.observe
  end

  include (M2 : Monad.S with type 'a t := 'a t
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

  let run m init =
    let comp =
      init_components () >>= fun () -> m >>= fun x ->
      Machine.Observation.make finish () >>= fun () ->
      Machine.return x in
    Machine.run comp init

end
