open Core_kernel.Std
open Regular.Std
open Bap.Std
open Graphlib.Std
open Monads.Std


module Multi = struct
  module type S2 = sig
    include Monad.Trans.S1
    type id

    type status = [`Current | `Live | `Dead]

    val global : id

    val fork : unit -> (unit,'e) t
    val switch : id -> (unit,'e) t
    val parent : unit -> (id,'e) t
    val ancestor : id list -> (id,'e) t
    val current : unit -> (id,'e) t
    val kill : id -> (unit,'e) t
    val forks : unit -> (int,'e) t
    val status : id -> (status,'e) t
    include Monad.S2 with type ('a,'e) t := ('a,'e) t
  end

  module Id = Int
  type id = Id.t


  type 'e contexts = {
    created : id;             (* the id of last created fork *)
    current : id;             (* the id of current context *)
    parents : id Id.Map.t;    (* tree of forks *)
    init  : 'e;                (* father of all forks *)
    forks : 'e Id.Map.t;       (* all forks of the Father  *)
  }

  module T2(M : Monad.S) = struct
    type ('a,'e) t = ('a,'e contexts) Monad.State.T2(M).t
    type ('a,'e) e = ('a,'e) Monad.State.T2(M).e
    type 'a m = 'a M.t
  end

  module Make2(M : Monad.S) : S2
    with type ('a,'e) t := ('a,'e) T2(M).t
     and type ('a,'e) e := ('a,'e) T2(M).e
     and type 'a m := 'a T2(M).m
  = struct
    module SM = struct
      include Monad.State.T2(M)
      include Monad.State.Make2(M)
    end

    open SM.Syntax
    type 'a m = 'a M.t
    type ('a,'e) state = ('a,'e contexts) SM.t
    type id = Id.t
    type ('a,'e) t = ('a,'e) state
    type ('a,'e) e = ('a,'e) SM.e
    type status = [`Current | `Live | `Dead]

    let global = Id.zero
    let init ctxt = {
      init = ctxt;
      created = global;
      current = global;
      parents = Id.Map.empty;
      forks = Id.Map.empty;
    }

    let rec alive_parent k child =
      match Map.find k.parents child with
      | None -> global
      | Some p when Map.mem k.forks p -> p
      | Some zombie -> alive_parent k zombie

    let rec ancestor k cs =
      match List.map cs ~f:(alive_parent k) with
      | [] -> global
      | p :: ps when List.for_all ps ~f:(fun p' -> p = p') -> p
      | ps -> ancestor k ps

    let ancestor cs = SM.gets @@ fun k -> ancestor k cs

    let alive k id : id  =
      if Map.mem k.forks id then id else alive_parent k id

    let forks () = SM.gets (fun k -> Map.length k.forks)

    let context k =
      let id = alive k k.current in
      if id = global then k.init
      else Map.find_exn k.forks k.current

    let fork () = SM.update @@ fun k ->
      let ctxt = context k in
      let current = Id.succ k.created in
      let parents = Map.add k.parents ~key:current ~data:k.current in
      let forks = Map.add k.forks ~key:current ~data:ctxt in
      {k with created=current; current; parents; forks}

    let switch id = SM.update @@ fun k -> {k with current = alive k id}
    let current () = SM.gets @@ fun k -> alive k k.current

    let get () : ('e,'e) t = SM.get () >>| context
    let put ctxt = SM.update @@ fun k ->
      let id = alive k k.current in
      if id = global then {k with init=ctxt} else {
        k with forks = Map.add k.forks ~key:id ~data:ctxt
      }

    let parent () = SM.get () >>| fun k -> alive_parent k k.current

    let status id = SM.get () >>| fun k ->
      if id = k.current then `Current else
      if Map.mem k.forks id then `Live else `Dead

    let remove_dead_parents k = {
      k with
      parents =
        Map.fold k.forks ~init:Id.Map.empty ~f:(fun ~key ~data:_ ps ->
              Map.add ps ~key ~data:(alive_parent k key))
    }

    let gc k =
      if k.created mod 1024 = 0 then remove_dead_parents k else k

    let kill id = SM.update @@ fun k ->
      gc {
        k with
        forks = Map.remove k.forks id;
        current = if id = k.current then alive_parent k id else id;
      }

    let gets f = get () >>| f
    let update f = get () >>= f >>= put
    let modify m f = m >>= fun x -> update f >>= fun () -> SM.return x


    let run (m : ('a,'e) state) = fun ctxt -> M.bind (SM.run m (init ctxt)) @@ fun (x,cs) ->
      M.return (x,cs.init)

    include Monad.Make2(struct
        type ('a,'e) t = ('a,'e) state
        let return = SM.return
        let bind = SM.bind
        let map = `Custom SM.map
      end)

    let lift m = SM.lift m
  end
end



module Nondet = struct

end
