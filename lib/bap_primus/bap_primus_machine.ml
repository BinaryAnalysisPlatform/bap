open Core_kernel[@@warning "-D"]
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
    ~desc:"Occurs just before the Machine exception is raised."

let fork,forked =
  Observation.provide
    ~inspect:(fun (parent,child) -> Sexp.List [
        Monad.State.Multi.Id.sexp_of_t parent;
        Monad.State.Multi.Id.sexp_of_t child;
      ])
    "machine-fork"
    ~desc:"Occurs after a machine fork is created."

let switch,switched =
  Observation.provide
    ~inspect:(fun (parent,child) -> Sexp.List [
        Monad.State.Multi.Id.sexp_of_t parent;
        Monad.State.Multi.Id.sexp_of_t child;
      ])
    "machine-switch"
    ~desc:"Occurs when machines are switched."


let kill,killed = Observation.provide "machine-kill"
    ~inspect:Monad.State.Multi.Id.sexp_of_t
    ~desc:"Occurs after the given machine is killed.
Observations are in the restricted mode."

let stop,stopped =
  Observation.provide ~inspect:sexp_of_string "system-stop"
    ~desc:"Occurs after the system is stopped. Observations
are in the restricted mode"

let start,started =
  Observation.provide ~inspect:sexp_of_string "system-start"
    ~desc:"Occurs after the system start."

module Make(M : Monad.S) = struct
  type id = Monad.State.Multi.id
  module Id = Monad.State.Multi.Id

  type r = (exit_status * project) M.t

  type state = {
    self    : id;
    args    : string array;
    envp    : string array;
    proj    : project;
    conts   : (unit -> unit machine) Map.M(Id).t;
    local   : State.Bag.t Map.M(Id).t;
    parent  : id Map.M(Id).t;
    global  : State.Bag.t;
    deathrow : id list;
    observations : unit machine Observation.observations;
    restricted : bool;
  }
  and 'a machine = {
    run :
      reject:(exn -> state -> r) ->
      accept:('a -> state -> r) ->
      state -> r
  }

  type 'a t = 'a machine
  type 'a m = 'a M.t
  type 'a e =
    ?boot:unit t ->
    ?init:unit t ->
    ?fini:unit t ->
    (exit_status * project) m effect_

  type _ error = exn

  module Machine = Monad.Make(struct
      type 'a t = 'a machine
      let return x : 'a t = {
        run = fun ~reject:_ ~accept s ->
          accept x s
      } [@@inline]

      let bind : 'a t -> ('a -> 'b t) -> 'b t = fun x f -> {
          run = fun ~reject ~accept s ->
            x.run s ~reject ~accept:(fun x s ->
                (f x).run ~reject ~accept s)
        } [@@inline]

      let map : 'a t -> f:('a -> 'b) -> 'b t = fun x ~f -> {
          run = fun ~reject ~accept s ->
            x.run s ~reject ~accept:(fun x s ->
                accept (f x) s)
        } [@@inline]

      let map = `Custom map
    end)
  open Machine

  open Machine.Syntax
  open Machine.Let

  let get () = {
    run = fun ~reject:_ ~accept s -> accept s s
  } [@@inline]

  let gets f = {
    run = fun ~reject:_ ~accept s -> accept (f s) s
  } [@@inline]

  let put s = {
    run = fun ~reject:_ ~accept _ -> accept () s
  } [@@inline]

  let update f = {
    run = fun ~reject:_ ~accept s -> accept () (f s)
  } [@@inline]

  let current () = gets (fun s -> s.self)
  let switch self = update (fun s -> {s with self})

  let with_context cid (f : (unit -> 'a t)) = {
    run = fun ~reject ~accept s ->
      (f ()).run {s with self=cid}
        ~reject
        ~accept:(fun r s'->
            accept r {s' with self=s.self})
  }

  let get_local () : _ t = gets (fun s ->
      match Map.find s.local s.self with
      | Some bag -> bag
      | None -> State.Bag.empty)

  let get_global () : _ t = gets (fun s -> s.global)

  let set_local local = update @@ fun s -> {
      s with local = Map.set s.local s.self local
    }

  let set_global global = update @@ fun s -> {
      s with global
    }

  let is_restricted () : bool t = gets @@ fun s -> s.restricted

  module Observation = struct
    type 'a m = 'a t
    type nonrec 'a observation = 'a observation
    type nonrec 'a statement = 'a statement

    let observations () = gets @@ fun s -> s.observations
    let unrestricted () = gets @@ fun s ->
      if s.restricted then None
      else Some s.observations

    let set_observations observations =
      update @@ fun s -> {s with observations}

    let subscribe key observer =
      observations () >>= fun os ->
      let obs,sub = Observation.add_observer os key observer in
      set_observations obs >>| fun () -> sub

    let observe key observer =
      subscribe key observer >>| fun _ -> ()

    let watch prov watcher =
      observations () >>= fun os ->
      let obs,_ = Observation.add_watcher os prov watcher in
      set_observations obs

    let cancel sub =
      observations () >>= fun os ->
      set_observations (Observation.cancel sub os)

    let obsname x = Observation.(name@@of_statement x)

    module Observation = Observation.Make(struct
        type 'a t = 'a machine
        include Machine
      end)

    let make key obs = unrestricted () >>= function
      | None -> return ()
      | Some os -> Observation.notify os key obs

    let make_even_if_restricted key obs =
      observations () >>= fun os ->
      Observation.notify os key obs

    let post key ~f =
      unrestricted () >>= function
      | None -> return ()
      | Some os ->
        Observation.notify_if_observed os key @@ fun k ->
        f (fun x -> k x)
  end

  let make_get states state =
    states () >>= fun states ->
    State.Bag.with_state states state
      ~ready:return
      ~create:(fun make ->
          get () >>= fun {proj} ->
          return (make proj))

  let make_put get set state x =
    get () >>= fun states ->
    set (State.Bag.set states state x)

  let make_update get put data ~f =
    get data >>= fun s -> put data (f s)


  module Local = struct
    let get s = make_get get_local s
    let put s = make_put get_local set_local s
    let update s = make_update get put s
  end

  module Other = struct
    let get_other pid state = with_context pid @@
      fun () -> get_local state
    let put_other pid state = with_context pid @@ fun () ->
      set_local state
    let get pid = make_get (get_other pid)
    let put pid = make_put (get_other pid) (put_other pid)
    let update pid = make_update (get pid) (put pid)
  end

  module Global = struct
    let get s = make_get get_global s
    let put s = make_put get_global set_global s
    let update s = make_update get put s
  end

  let global = Id.zero

  let next_self s =
    match Map.max_elt s.parent with
    | None -> Id.succ global
    | Some (id,_) -> Id.succ id

  let fork_state () = update @@ fun s ->
    let child = next_self s in {
      s with
      local = Map.change s.local child ~f:(fun _ ->
          Map.find s.local s.self);
      self = child;
      parent = Map.set s.parent child s.self;
    }

  let switch_state self : unit t = update @@ fun s -> {s with self}
  let store_curr k = update @@ fun s -> {
      s with conts = Map.set s.conts s.self k
    }

  let get_curr = gets @@ fun {conts; self} ->
    match Map.find conts self with
    | None -> return
    | Some k -> k


  let status x = gets @@ fun {self; conts} ->
    if Id.equal self x then `Current else
    if Map.mem conts x then `Live else `Dead


  let forks () = gets @@ fun s ->
    let fs = Map.to_sequence s.conts |> Sequence.map ~f:fst in
    Sequence.shift_right fs global

  let ancestor xs = gets @@ fun {parent=ps; conts} ->
    let is_alive = Map.mem conts in
    let rec parent i = match Map.find ps i with
      | None -> global
      | Some p when is_alive p -> p
      | Some p -> parent p in
    let rec common xs =
      match Base.List.map xs ~f:parent with
      | [] -> global
      | p :: ps when Base.List.for_all ps ~f:(Id.equal p) -> p
      | ps -> common ps in
    common xs

  let current () = gets @@ fun s -> s.self

  let parent () =
    let* id = current () in
    ancestor [id]

  let notify_fork pid =
    current () >>= fun cid ->
    Observation.make forked (pid,cid)

  let call : f:(cc:('a -> _ t) -> 'a t) -> 'a t = fun ~f -> {
      run = fun ~reject ~accept s ->
        let cc x = {run = fun ~reject:_ ~accept:_ s -> accept x s} in
        (f ~cc).run ~reject ~accept s
    }

  let restrict x = update @@ fun s -> {
      s with restricted = x
    }

  let sentence_to_death id = update @@ fun s -> {
      s with deathrow = id :: s.deathrow
    }

  let drop id s = {
    s with
    conts = Map.remove s.conts id;
    local = Map.remove s.local id;
  }

  let do_kill id = update (drop id)

  let execute_sentenced = update @@ fun s ->
    Base.List.fold s.deathrow ~init:{s with deathrow=[]} ~f:(fun s id ->
        drop id s)

  let switch id : unit t =
    is_restricted () >>= function
    | true -> failwith "switch in the restricted mode"
    | false ->
      call ~f:(fun ~cc:k ->
          current () >>= fun pid ->
          store_curr k >>= fun () ->
          switch_state id >>= fun () ->
          get_curr >>= fun k ->
          execute_sentenced >>= fun () ->
          Observation.make switched (pid,id) >>= fun () ->
          k ())


  let fork () : unit t =
    is_restricted () >>= function
    | true -> failwith "fork in the restricted mode"
    | false ->
      call ~f:(fun ~cc:k ->
          current () >>= fun pid ->
          store_curr k >>=
          fork_state >>= fun () ->
          execute_sentenced >>= fun () ->
          notify_fork pid)


  let kill id =
    is_restricted () >>= function
    | true -> failwith "kill in the restricted mode"
    | false ->
      if Id.(id = global) then return ()
      else
        current () >>= fun cid ->
        if Id.(id = cid)
        then
          restrict true >>= fun () ->
          Observation.make_even_if_restricted killed id >>= fun () ->
          restrict false >>= fun () ->
          sentence_to_death id
        else
          switch id >>= fun () ->
          restrict true >>= fun () ->
          Observation.make_even_if_restricted killed id >>= fun () ->
          restrict false >>= fun () ->
          switch cid >>= fun () ->
          do_kill id

  let start sys =
    Observation.make started sys

  let stop sys =
    restrict true >>= fun () ->
    Observation.make_even_if_restricted stopped sys

  let fail exn = {
    run = fun ~reject ~accept:_ s -> reject exn s
  }

  let catch x except = {
    run = fun ~reject ~accept s ->
      x.run s
        ~accept
        ~reject:(fun exn s ->
            (except exn).run ~reject ~accept s)
  }

  let lift : 'a m -> 'a t = fun x -> {
      run = fun ~reject:_ ~accept s ->
        M.bind x ~f:(fun x -> accept x s)
    }

  let raise exn =
    Observation.make raise_exn exn >>= fun () ->
    fail exn
  let catch = catch

  let args = gets @@ fun s -> s.args
  let envp = gets @@ fun s -> s.envp

  let put proj = update @@ fun s -> {s with proj}
  let get () = gets @@ fun s -> s.proj
  let project : project t = get ()
  let gets f = get () >>| f
  let update f = get () >>= fun s -> put (f s)
  let modify m f = m >>= fun x -> update f >>= fun () -> return x

  let project = get ()
  let program = project >>| Project.program
  let arch = project >>| Project.arch


  let init_state proj ~args ~envp = {
    args;
    envp;
    self = global;
    conts = Map.empty (module Id);
    local = Map.empty (module Id);
    parent = Map.empty (module Id);
    global = State.Bag.empty;
    observations = Bap_primus_observation.empty;
    deathrow = [];
    proj;
    restricted = true;
  }

  let exec m s = m.run s
      ~accept:(fun _ s -> M.return (Normal, s.proj))
      ~reject:(fun exn s -> M.return (Exn exn,s.proj))

  let run : 'a t -> 'a e =
    fun user
      ?(boot=return ())
      ?(init=return ())
      ?(fini=return ())
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
          (fun exn ->
             fini >>= fun () ->
             stop sys >>= fun () ->
             raise exn) >>= fun x ->
        fini >>= fun () ->
        stop sys >>= fun () ->
        return x in
      exec machine (init_state proj ~args ~envp)


  module Syntax = struct
    include Machine.Syntax
    let (>>>) = Observation.observe
  end

  include (Machine : Monad.S with type 'a t := 'a t
                              and module Syntax := Syntax)
end
