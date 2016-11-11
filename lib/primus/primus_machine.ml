open Core_kernel.Std
open Bap.Std
open Monads.Std
open Primus_types

module Error = Primus_error
module Observation = Primus_observation

let components : component list ref = ref []
let add_component comp = components := comp :: !components

module type Component = Component
module type S = Machine
type nonrec component = component


module State = Primus_state

module Make(M : Monad.S)
= struct
  module SM = struct
    include Monad.State.Multi.T2(M)
    include Monad.State.Multi.Make2(M)
  end

  type ('a,'e) t = (('a,Error.t) result,'e state) SM.t
  and 'e state = {
    ctxt : 'e;
    local  : State.Bag.t;
    global : State.Bag.t;
    states : int String.Map.t;
    observations : 'e observations;
  }
  and 'e observations = (unit,'e) t Observation.observations

  type 'a m = 'a M.t
  type ('a,'e) e = 'e -> (('a,Error.t) result * 'e) m
  module Basic = struct
    open SM.Syntax
    type nonrec ('a,'e) t = ('a,'e) t
    let return x = SM.return (Ok x)

    let bind (m : ('a,'e) t) (f : 'a -> ('b,'e) t) : ('b,'e) t = m >>= function
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
  include Monad.Make2(Basic)

  type id = Monad.State.Multi.id
  module Id = Monad.State.Multi.Id

  let lifts m = SM.map m ~f:(fun x -> Ok x)

  let with_global_context (f : (unit -> ('a,'b) t)) =
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
    type ('a,'e) m = ('a,'e) t
    type nonrec 'a observation = 'a observation
    type nonrec 'a statement = 'a statement

    let observations () = lifts (SM.gets @@ fun s -> s.observations)

    let set_observations observations = with_global_context @@ fun () ->
      lifts (SM.update @@ fun s -> {s with observations})

    let make key obs =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      Observation.with_observers os key ~f:(List.iter ~f:(fun observe -> observe obs))

    let observe key observer =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      set_observations (Observation.add_observer os key observer)
  end

  module Make_state(S : sig
      val get : unit -> (State.Bag.t,'e) t
      val set : State.Bag.t -> (unit,'e) t
      val typ : string
    end) = struct
    type ('a,'e) m = ('a,'e) t
    let get state =
      S.get () >>= fun states ->
      State.Bag.with_state states state
        ~ready:return
        ~create:(fun make ->
            lifts (SM.get ()) >>= fun {ctxt} ->
            return (make (ctxt :> Context.t)))

    let put state x =
      S.get () >>= fun states -> S.set (State.Bag.set states state x)

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

  let put ctxt = lifts @@ SM.update @@ fun s -> {s with ctxt}
  let get () = lifts (SM.gets @@ fun s -> s.ctxt)
  let gets f = get () >>| f
  let update f = get () >>= fun s -> put (f s)
  let modify m f = m >>= fun x -> update f >>= fun () -> return x

  let run : ('a,'e) t -> ('a,'e) e = fun m ctxt ->
    M.bind (SM.run m {
        global = State.Bag.empty;
        local = State.Bag.empty;
        observations = Primus_observation.empty;
        states = String.Map.empty;
        ctxt}) @@ fun (x,{ctxt}) -> M.return (x,ctxt)

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
