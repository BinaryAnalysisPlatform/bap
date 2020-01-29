open Core_kernel
open Monads.Std
open Bap_knowledge


type outcome =
  | Continue
  | Stop
  | Info of Info.t

module Observer = struct
  type ('f,'m) t = {
    id : int;
    observe: (outcome -> 'm) -> 'f
  }
end

module Inspector = struct
  type 'm t = {
    id : int;
    inspect : Info.t -> 'm
  }
end

module Observation = struct
  type info = Info.t


  type ('f,'m) t = {
    name : string;
    inspect : ((info -> 'm) -> 'f) option;
    observer : ('f,'m) Observer.t Univ_map.Multi.Key.t;
  }

  let declare ?inspect ?(package="user") name =
    let name = sprintf "%s:%s" package name in
    let observer = Univ_map.Multi.Key.create ~name sexp_of_opaque in
    {name; inspect; observer}
end

module State = struct
  type 'a t = {
    key : 'a Univ_map.Key.t;
    init : (unit -> unit) Knowledge.obj -> 'a knowledge;
    inspect : ('a -> Info.t);
  }
  type 'a state = 'a t

  let declare ?(inspect=fun _ -> Info.of_string "<opaque>") ?
      (name="anonymous") init =
    let sexp_of_t x = Info.sexp_of_t (inspect x) in
    let key = Type_equal.Id.create ~name sexp_of_t in
    {key; init; inspect}

  let inspect x = x.inspect
  let name x = Type_equal.Id.name x.key
end

module Exception : sig
  type t = ..
  val to_string : t -> string
  val add_printer : (t -> string option) -> unit
end
= struct
  type t = exn = ..
  let to_string err = Caml.Printexc.to_string err
  let add_printer pr = Caml.Printexc.register_printer pr
end


module Primus = struct
  type id = Monad.State.Multi.id

  type project = unit -> unit
  let project = Knowledge.Class.declare ~package:"primus" "project" ()


  type env = unit
  type exn = Exception.t = ..

  let package = "primus"

  module PE = struct
    type t = (unit, exn) Monad.Result.result
  end

  module SM = struct
    include Monad.State.Multi.T2(Knowledge)
    include Monad.State.Multi.Make2(Knowledge)
  end

  type exit_status =
    | Normal
    | Exn of Exception.t

  type 'a state = 'a State.t
  type 'a t  = (('a,exn) result,PE.t sm) Monad.Cont.t
  and 'f observation = ('f, outcome t) Observation.t
  and 'f observer = ('f, outcome t) Observer.t
  and inspectors = outcome t Inspector.t list
  and 'a sm  = ('a,machine_state) SM.t
  and machine_state = {
    proj    : project Knowledge.obj;
    curr    : unit -> unit t;
    local   : Univ_map.t;
    global  : Univ_map.t;
    deathrow : id list;
    observations : Univ_map.t;
    inspectors : inspectors Map.M(String).t;
    key : int;
  }


  type 'a c = 'a t
  type 'a m = 'a Knowledge.t
  type 'a e = project Knowledge.obj -> unit m


  module C = Monad.Cont.Make(PE)(struct
      type 'a t = 'a sm
      include Monad.Make(struct
          type 'a t = 'a sm
          let return = SM.return
          let bind m f = SM.bind m ~f
          let map = `Custom SM.map
        end)
    end)

  module CM = Monad.Result.Make(Exception)(struct
      type 'a t = ('a, PE.t sm) Monad.Cont.t
      include C
    end)

  type _ error = exn
  open CM

  module Id = Monad.State.Multi.Id

  type 'a machine = 'a t


  let exn_raised = Observation.declare ~package "machine-exception"
      ~inspect:(fun k exn ->
          k (Info.of_string (Exception.to_string exn)))

  let forked = Observation.declare ~package "machine-fork"
      ~inspect:(fun k pid cid ->
          k @@ Info.create "machine-fork" (pid,cid)
            [%sexp_of:  Id.t * Id.t])

  let switched = Observation.declare ~package "machine-switch"
      ~inspect:(fun k parent child ->
          k @@ Info.create "machine-switch" (parent,child)
            [%sexp_of: Id.t * Id.t])


  let liftk x = CM.lift (C.lift (SM.lift x))
  (* lifts state monad to the outer monad *)
  let lifts x = CM.lift (C.lift x)
  let fact = liftk

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

  let project = lifts (SM.gets @@ fun s -> s.proj)

  type observed = outcome
  module Observation : sig
    type 'f t = 'f observation
    type info = Info.t
    type ctrl
    type observed = outcome

    val declare :
      ?inspect:((info -> observed machine) -> 'f) ->
      ?package:string -> string ->
      'f observation
    val provide : 'f observation -> f:('f -> observed machine) -> unit machine
    val monitor : 'f observation -> f:(ctrl -> 'f) -> unit machine
    val inspect : 'f observation -> f:(info -> observed machine) -> unit machine
    val continue : ctrl -> observed machine
    val stop : ctrl -> observed machine
  end
  = struct
    type 'a m = 'a t
    type 'f t = 'f observation
    type observed = outcome
    type info = Info.t
    type ctrl = outcome -> outcome machine

    open Observation

    let declare = Observation.declare
    let empty = Set.empty (module Int)

    let observations {observer} : _ observer list m =
      lifts @@ SM.gets @@ fun s ->
      Univ_map.Multi.find s.observations observer

    let set_observations {observer} obs =
      lifts @@ SM.update @@ fun s -> {
        s with
        observations = Univ_map.Multi.set s.observations observer obs
      }

    let set_inspectors {Observation.name} data =
      lifts @@ SM.update @@ fun s -> {
        s with
        inspectors = Map.set s.inspectors ~key:name ~data
      }

    let inspectors {name} =
      lifts @@ SM.gets @@ fun {inspectors} ->
      Map.find_multi inspectors name

    (* we don't want to use List.iter as it will create
       extra allocations.

       Accumulates the kill set - ids of observers that
       opted for unsubscription. *)
    let rec loop kill k = function
      | [] -> return kill
      | {Observer.id; observe} :: fs ->
        k (observe return) >>= function
        | Stop -> loop (Set.add kill id) k fs
        | _ -> loop kill k fs


    let get_info {Observation.name; inspect} k =
      let noinfo = Info.of_string name in
      match inspect with
      | None -> return noinfo
      | Some inspect ->
        k (inspect (fun data -> Info data)) >>| function
        | Info data -> data
        | _ -> noinfo
    (* in case if the inspector didn't call our continuation *)

    let call_inspectors obs k =
      inspectors obs >>= function
      | [] -> return ()
      | fs -> get_info obs k >>= fun info ->
        List.fold ~init:[] fs ~f:(fun fs ({inspect} as f) ->
            inspect info >>= function
            | Stop -> return fs
            | _ -> return (f::fs)) >>=
        set_inspectors obs

    let kill_observers observers kill =
      Base.List.rev_filter observers ~f:(fun {Observer.id} ->
          not (Set.mem kill id))

    let provide observation ~f:k =
      observations observation >>= fun obs ->
      loop empty k obs >>= fun kill ->
      if Set.is_empty kill then return ()
      else
        kill_observers obs kill |>
        set_observations observation

    let monitor {observer} ~f =
      lifts @@ SM.update @@ fun s -> {
        s with
        key = s.key + 1;
        observations = Univ_map.Multi.add s.observations observer
            Observer.{
              id = s.key + 1;
              observe = f;
            }
      }

    let inspect {name} ~f =
      lifts @@ SM.update @@ fun s -> {
        s with
        key = s.key + 1;
        inspectors = Map.add_multi s.inspectors ~key:name
            ~data:Inspector.{
                id = s.key + 1;
                inspect = f;
              }
      }

    let continue k = k Continue
    let stop k = k Stop
  end

  module type State = sig
    val get : 'a state -> 'a machine
    val put : 'a state -> 'a -> unit machine
    val update : 'a state -> f:('a -> 'a) -> unit machine
  end
  module Make_state(S : sig
      val get : unit -> Univ_map.t t
      val set : Univ_map.t -> unit t
      val typ : string
    end) = struct
    type 'a m = 'a t
    let get : 'a state -> 'a machine = fun {State.key; init} ->
      S.get () >>= fun states ->
      match Univ_map.find states key with
      | Some s -> return s
      | None ->
        project >>= fun proj ->
        liftk (init proj)

    let put {State.key} x  =
      S.get () >>= fun states ->
      S.set (Univ_map.set states key x)

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

  module State = State

  let get () = CM.return ()
  let put () : unit machine = CM.return ()
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
    Observation.provide forked ~f:(fun observe ->
        observe pid cid)

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
        Observation.provide switched ~f:(fun observe ->
            observe pid id) >>= fun () ->
        s.curr ())


  let fork () : unit c =
    C.call ~f:(fun ~cc:k ->
        current () >>= fun pid ->
        store_curr k >>=
        fork_state >>= fun () ->
        execute_sentenced >>= fun () ->
        notify_fork pid)


  let kill id =
    if Id.(id = global) then return ()
    else
      current () >>= fun cid ->
      if Id.(id = cid) then sentence_to_death id
      else lifts @@ SM.kill id

  let die next =
    current () >>= fun pid ->
    switch_state next >>= fun () ->
    lifts (SM.get ()) >>= fun s ->
    lifts (SM.kill pid) >>= fun () ->
    s.curr ()


  let raise exn =
    Observation.provide exn_raised ~f:(fun observe ->
        observe exn) >>= fun () ->
    fail exn
  let catch = catch

  let empty proj = {
    proj;
    curr = return;
    global = Univ_map.empty;
    local = Univ_map.empty;
    observations = Univ_map.empty;
    inspectors = Map.empty (module String);
    key = 0;
    deathrow = [];
  }


  let finished =
    Observation.declare ~package "fini"
      ~inspect:(fun k () -> k @@ Info.of_string "fini")

  let inited = Observation.declare ~package "init"
      ~inspect:(fun k () -> k @@ Info.of_string "init")

  let notify obs = Observation.provide obs ~f:(fun go -> go ())


  let run : type a. a t -> a e = fun comp proj ->
    let finish = function
      | Ok _ -> SM.return (Ok ())
      | Error err -> SM.return (Error err) in
    let state = empty proj in
    Knowledge.ignore_m @@
    SM.run (C.run comp finish) state

  module Syntax = struct
    include CM.Syntax
    let (-->) x p = collect p x
    let (//)  c s = liftk @@ Knowledge.Object.read c s
    let (>>>) x f = Observation.monitor x ~f
  end

  include (CM : Monad.S with type 'a t := 'a t
                         and module Syntax := Syntax)
end
