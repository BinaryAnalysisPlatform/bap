open Core_kernel
open Monads.Std
open Bap_knowledge
open Bap_primus_types
open Format

module type S = Machine
module State = Bap_primus_state
type id = Monad.State.Multi.id
type env = unit

let exn_raised,raise_exn =
  Bap_primus_observation.provide
    ~inspect:(fun exn -> Sexp.Atom (Bap_primus_exn.to_string exn))
    "exception"

let fork,forked =
  Bap_primus_observation.provide
    ~inspect:(fun (parent,child) -> Sexp.List [
        Monad.State.Multi.Id.sexp_of_t parent;
        Monad.State.Multi.Id.sexp_of_t child;
      ])
    "machine-fork"


let switch,switched =
  Bap_primus_observation.provide
    ~inspect:(fun (parent,child) -> Sexp.List [
        Monad.State.Multi.Id.sexp_of_t parent;
        Monad.State.Multi.Id.sexp_of_t child;
      ])
    "machine-switch"

module PE = struct
  type t = (unit, exn) Monad.Result.result
end

module SM = struct
  include Monad.State.Multi.T2(Knowledge)
  include Monad.State.Multi.Make2(Knowledge)
end

type 'a t  = (('a,exn) result,PE.t sm) Monad.Cont.t
and 'a sm  = ('a,state) SM.t
and state = {
  curr    : unit -> unit t;
  local   : State.Bag.t;
  global  : State.Bag.t;
  deathrow : id list;
  observations : unit t Bap_primus_observation.observations;
}

type 'a c = 'a t
type 'a m = 'a Knowledge.t
type 'a e =
  unit t service list ->
  knowledge ->
  (exit_status * knowledge, conflict) result


module C = Monad.Cont.Make(PE)(struct
    type 'a t = 'a sm
    include Monad.Make(struct
        type 'a t = 'a sm
        let return = SM.return
        let bind m f = SM.bind m ~f
        let map = `Custom SM.map
      end)
  end)

module CM = Monad.Result.Make(Exn)(struct
    type 'a t = ('a, PE.t sm) Monad.Cont.t
    include C
  end)

type _ error = exn
open CM

module Id = Monad.State.Multi.Id

let liftk x = CM.lift (C.lift (SM.lift x))
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
    Seq.sequence @@ Bap_primus_observation.notify os key obs

  let observe key observer =
    with_global_context @@ fun () ->
    observations () >>= fun os ->
    set_observations (Bap_primus_observation.add_observer os key observer)

  let watch prov watcher =
    with_global_context @@ fun () ->
    observations () >>= fun os ->
    set_observations (Bap_primus_observation.add_watcher os prov watcher)
end

module Make_state(S : sig
    val get : unit -> State.Bag.t t
    val set : State.Bag.t -> unit t
    val typ : string
  end) = struct
  type 'a m = 'a t
  let get state : 'a c =
    S.get () >>= fun states ->
    State.Bag.with_state states state
      ~ready:return
      ~create:liftk

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

let get () = CM.return ()
let put () = CM.return ()
let gets f = CM.return (f ())
let update _ = CM.return ()
let modify m _f = m


let provide c l x : unit t = liftk (Knowledge.provide c l x)
let collect c l : 'a t = liftk (Knowledge.collect c l)
let conflict c : 'a t = liftk (Knowledge.fail c)
let knowledge : 'a Knowledge.t -> 'a t = liftk

let fork_state () = lifts (SM.fork ())
let switch_state id : unit c = lifts (SM.switch id)
let store_curr k =
  lifts (SM.update (fun s -> {s with curr = fun () -> k (Ok ())}))

let lift x = lifts (SM.lift x)
let status x = lifts (SM.status x)
let forks () = lifts (SM.forks ())
let ancestor x  = lifts (SM.ancestor x)
let parent () = lifts (SM.parent ())
let global = SM.global
let current () = lifts (SM.current ())

let notify_fork pid =
  current () >>= fun cid ->
  Observation.make forked (pid,cid)

let sentence_to_death id =
  with_global_context (fun () ->
      lifts @@ SM.update (fun s -> {
            s with deathrow = id :: s.deathrow
          }))

let execute_sentenced =
  with_global_context (fun () ->
      lifts @@ SM.get () >>= fun s ->
      lifts @@ SM.List.iter s.deathrow ~f:SM.kill >>= fun () ->
      lifts @@ SM.put {s with deathrow = []})

let switch id : unit c =
  C.call ~f:(fun ~cc:k ->
      current () >>= fun pid ->
      store_curr k >>= fun () ->
      switch_state id >>= fun () ->
      lifts (SM.get ()) >>= fun s ->
      execute_sentenced >>= fun () ->
      Observation.make switched (pid,id) >>= fun () ->
      s.curr ())


let fork () : unit c =
  C.call ~f:(fun ~cc:k ->
      current () >>= fun pid ->
      store_curr k >>=
      fork_state >>= fun () ->
      execute_sentenced >>= fun () ->
      notify_fork pid)


let kill id =
  if id = global then return ()
  else
    current () >>= fun cid ->
    if id = cid then sentence_to_death id
    else lifts @@ SM.kill id

let die next =
  current () >>= fun pid ->
  switch_state next >>= fun () ->
  lifts (SM.get ()) >>= fun s ->
  lifts (SM.kill pid) >>= fun () ->
  s.curr ()


let raise exn =
  Observation.make raise_exn exn >>= fun () ->
  fail exn
let catch = catch

let empty = {
  curr = return;
  global = State.Bag.empty;
  local = State.Bag.empty;
  observations = Bap_primus_observation.empty;
  deathrow = [];
}


let finished,finish =
  Bap_primus_observation.provide ~inspect:sexp_of_unit "fini"

let init,inited =
  Bap_primus_observation.provide ~inspect:sexp_of_unit "init"

let with_components cs m : 'a t =
  let open CM.Syntax in
  CM.List.iter cs ~f:(fun {init} -> init) >>= fun () ->
  Observation.make inited () >>= fun () ->
  catch m (fun err ->
      Observation.make finish () >>= fun () ->
      raise err) >>= fun x ->
  Observation.make finish () >>= fun () ->
  return x

let run : type a. a t -> a e =
  fun m cs k ->
    let res = Knowledge.run
        (SM.run
           (C.run (with_components cs m) (function
                | Ok _ -> SM.return (Ok ())
                | Error err -> SM.return (Error err)))
           empty)
        k in
    match res with
    | Ok ((Ok (),_),k) -> Ok (Normal,k)
    | Ok ((Error e,_),k) -> Ok (Exn e, k)
    | Error c        -> Error c

module Syntax = struct
  include CM.Syntax
  let (>>>) = Observation.observe
end


type 'a machine = 'a t

module Component = struct
  type t = unit machine service
  let components : t list ref = ref []
  let provide ?(desc="not provided") ~name init =
    components := {name; desc; init} :: !components
  let list () = !components

  let name t = t.name
  let desc t = t.desc
end

include (CM : Monad.S with type 'a t := 'a t
                       and module Syntax := Syntax)
