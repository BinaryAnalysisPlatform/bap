open Core_kernel
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

let exn_raised,raise_exn =
  Observation.provide
    ~inspect:(fun exn -> Sexp.Atom (Bap_primus_exn.to_string exn))
    "exception"

let fork,forked =
  Observation.provide
    ~inspect:(fun (parent,child) -> Sexp.List [
        Monad.State.Multi.Id.sexp_of_t parent;
        Monad.State.Multi.Id.sexp_of_t child;
      ])
    "machine-fork"

let switch,switched =
  Observation.provide
    ~inspect:(fun (parent,child) -> Sexp.List [
        Monad.State.Multi.Id.sexp_of_t parent;
        Monad.State.Multi.Id.sexp_of_t child;
      ])
    "machine-switch"

let kill,killed = Observation.provide "machine-killed"
    ~inspect:Monad.State.Multi.Id.sexp_of_t


let stop,stopped =
  Observation.provide ~inspect:sexp_of_string "system-stopped"

let start,started =
  Observation.provide ~inspect:sexp_of_string "system-started"

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
    curr    : unit -> unit t;
    proj    : project;
    local   : State.Bag.t;
    global  : State.Bag.t;
    deathrow : id list;
    observations : unit t Observation.observations;
    restricted : bool;
  }

  type 'a machine = 'a t
  type 'a c = 'a t
  type 'a m = 'a M.t
  type 'a e =
    ?boot:unit t ->
    ?init:unit t ->
    (exit_status * project) m effect

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

  let is_restricted () : bool t = lifts (SM.gets @@ fun s -> s.restricted)

  module Observation = struct
    type 'a m = 'a t
    type nonrec 'a observation = 'a observation
    type nonrec 'a statement = 'a statement

    let observations () = lifts (SM.gets @@ fun s -> s.observations)
    let unrestricted () =
      lifts (SM.gets @@ fun s ->
             if s.restricted then None
             else Some s.observations)

    let set_observations observations = with_global_context @@ fun () ->
      lifts (SM.update @@ fun s -> {s with observations})

    let subscribe key observer =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      let obs,sub = Observation.add_observer os key observer in
      set_observations obs >>| fun () -> sub

    let observe key observer =
      subscribe key observer >>| fun _ -> ()

    let watch prov watcher =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      let obs,_ = Observation.add_watcher os prov watcher in
      set_observations obs

    let cancel sub =
      with_global_context @@ fun () ->
      observations () >>= fun os ->
      set_observations (Observation.cancel sub os)

    module Observation = Observation.Make(struct
        type 'a t = 'a machine
        include CM
      end)

    let make key obs =
      with_global_context unrestricted >>= function
      | None -> return ()
      | Some os -> Observation.notify os key obs

    let make_even_if_restricted key obs =
      with_global_context observations >>= fun os ->
      Observation.notify os key obs


    let post key ~f =
      with_global_context unrestricted >>= function
      | None -> return ()
      | Some os ->
        Observation.notify_if_observed os key @@ fun k ->
        f (fun x -> k x)
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

  let put proj = with_global_context @@ fun () ->
    lifts @@ SM.update @@ fun s -> {s with proj}
  let get () = with_global_context @@ fun () ->
    lifts (SM.gets @@ fun s -> s.proj)
  let project : project t = get ()
  let gets f = get () >>| f
  let update f = get () >>= fun s -> put (f s)
  let modify m f = m >>= fun x -> update f >>= fun () -> return x


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

  let restrict x = lifts @@ SM.update (fun s -> {
        s with restricted = x;
      })

  let do_kill id =
    restrict true >>= fun () ->
    Observation.make_even_if_restricted killed id >>= fun () ->
    lifts @@ SM.kill id


  let execute_sentenced =
    with_global_context (fun () ->
        lifts @@ SM.get () >>= fun s ->
        CM.List.iter s.deathrow ~f:do_kill >>= fun () ->
        lifts @@ SM.put {s with deathrow = []})

  let switch id : unit c =
    is_restricted () >>= function
    | true -> failwith "a non-determistic operation in the restricted mode"
    | false ->
      C.call ~f:(fun ~cc:k ->
          current () >>= fun pid ->
          store_curr k >>= fun () ->
          switch_state id >>= fun () ->
          lifts (SM.get ()) >>= fun s ->
          execute_sentenced >>= fun () ->
          Observation.make switched (pid,id) >>= fun () ->
          s.curr ())


  let fork () : unit c =
    is_restricted () >>= function
    | true ->
      failwith "a non-deterministic operation in the restricted mode"
    | false ->
      C.call ~f:(fun ~cc:k ->
          current () >>= fun pid ->
          store_curr k >>=
          fork_state >>= fun () ->
          execute_sentenced >>= fun () ->
          notify_fork pid)

  let die next =
    is_restricted () >>= function
    | true ->
      failwith "a non-deterministic operation in the restricted mode"
    | false ->
      current () >>= fun pid ->
      switch_state next >>= fun () ->
      lifts (SM.get ()) >>= fun s ->
      do_kill pid >>= fun () ->
      s.curr ()

  let kill id =
    is_restricted () >>= function
    | true ->
      failwith "a non-deterministic operation in the restricted mode"
    | false ->
      if Id.(id = global) then return ()
      else
        current () >>= fun cid ->
        if Id.(id = cid)
        then sentence_to_death id
        else do_kill id

  let start sys =
    Observation.make started sys

  let stop sys =
    restrict true >>= fun () ->
    Observation.make_even_if_restricted stopped sys

  let raise exn =
    Observation.make raise_exn exn >>= fun () ->
    fail exn
  let catch = catch

  let project = get ()
  let program = project >>| Project.program
  let arch = project >>| Project.arch
  let args = lifts (SM.gets @@ fun s -> s.args)
  let envp = lifts (SM.gets @@ fun s -> s.envp)


  let init_state proj ~args ~envp = {
    args;
    envp;
    curr = return;
    global = State.Bag.empty;
    local = State.Bag.empty;
    observations = Bap_primus_observation.empty;
    deathrow = [];
    proj;
    restricted = true;
  }

  let extract f =
    let open SM.Syntax in
    SM.switch SM.global >>= fun () ->
    SM.gets f

  let run : 'a t -> 'a e =
    fun user
      ?(boot=return ())
      ?(init=return ())
      ?(envp=[||])
      ?(args=[||])
      sys
      proj ->
      let machine =
        boot >>= fun () ->
        restrict false >>= fun () ->
        init >>= fun () ->
        catch
          (start sys >>= fun () -> user)
          (fun exn -> stop sys >>= fun () -> raise exn) >>= fun x ->
        stop sys >>= fun () ->
        return x in
      M.bind
        (SM.run
           (C.run machine (function
                | Ok _ -> extract @@ fun s -> Ok s.proj
                | Error err -> extract @@ fun _ -> Error err))
           (init_state proj ~args ~envp))
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
