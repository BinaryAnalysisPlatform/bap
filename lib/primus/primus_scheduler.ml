open Core_kernel.Std
open Bap.Std
open Monads.Std

open Primus_types
module Iterator = Primus_iterator
module RNG = Primus_random

module Scheduler = struct
  module type S = sig
    type  t
    type ('a,'e) m
    val schedule : t -> (t,#Biri.context) m
  end
end

(* continue with the same context, until a path terminates,
   then switch to a next thread that is not yet terminated.

   A thread  is terminated, if [ctxt#next = None]
*)
module Greedy = struct
  module Make(SM : Monad.State.Multi.S2) : Scheduler.S
    with type ('a,'e) m := ('a,'e) SM.t =  struct
    open SM.Syntax

    type ('a,'e) m = ('a,'e) SM.t constraint 'e = #Biri.context

    type t = unit

    let schedule () =
      SM.get () >>= fun ctxt -> match ctxt#next with
      | Some _ -> SM.return ()
      | None -> SM.forks () >>= SM.Seq.find ~f:(fun id ->
            SM.switch id >>= fun () ->
            SM.get () >>| fun ctxt -> match ctxt#next with
            | None -> false
            | Some _ -> true) >>| ignore
  end
end

module RR = struct
  module Make(SM : Monad.State.Multi.S2) : Scheduler.S = struct
    open SM.Syntax

    type ('a,'e) m = ('a,'e) SM.t

    type t = {
      pending : SM.id Fqueue.t
    }

    let rec schedule t = match Fqueue.dequeue t.pending with
      | None -> SM.forks () >>= fun fs -> schedule {
          pending = Seq.fold fs ~init:Fqueue.empty ~f:Fqueue.enqueue
        }
      | Some (next,pending) -> SM.status next >>= function
        | `Dead -> schedule {pending}
        | _ -> SM.switch next >>| fun () -> {pending}
  end
end




(* will prefer threads that will wonder into less explored areas *)
module Explorer = struct

  module Make(SM : Monad.State.Multi.S2) = struct
    open SM.Syntax

    type t = {
      explored : int Tid.Map.t;
      pending  : SM.id Fqueue.t;
    }

    let enqueue level t id  =
      let add tid = {
        pending = Fqueue.enqueue t.pending id;
        explored = Map.add t.explored ~key:tid ~data:0
      } in
      SM.switch id >>= fun () ->
      SM.get () >>| fun ctxt -> match ctxt#next with
      | None -> t
      | Some tid -> match Map.find t.explored tid with
        | None -> add tid
        | Some 0 -> t
        | Some n when n = level -> add tid
        | Some _ -> t

    let remove_planned_explorations t = {
      t with explored = Map.filteri t.explored
                 ~f:(fun ~key ~data -> data <> 0)
    }

    let rec reschedule level t =
      SM.current () >>= fun id ->
      SM.forks () >>=
      SM.Seq.fold ~init:t ~f:(enqueue level) >>= fun t ->
      SM.switch id >>= fun () ->
      if Fqueue.is_empty t.pending then reschedule (level+1) t
      else SM.return @@ remove_planned_explorations t

    let rec schedule t =
      match Fqueue.dequeue t.pending with
      | None -> reschedule 0 t >>= schedule
      | Some (id,pending) ->
        SM.get () >>= fun ctxt -> match ctxt#trace with
        | [] -> SM.switch id >>| fun () -> {t with pending}
        | curr::_ -> SM.switch id >>| fun () -> {
            pending;
            explored = Map.update t.explored curr ~f:(function
                | None -> 1
                | Some n -> n+1)
        }
  end
end

(* pick the next thread on a random basis *)
module Random = struct
  module Make
      (Dom : Int_intf.S)
      (Rng : Iterator.Infinite.S with type dom = Dom.t)
      (SM : Monad.State.Multi.S2) : Scheduler.S = struct
    open SM.Syntax

    let attempts = 4

    type ('a,'e) m = ('a,'e) SM.t

    type t = {
      cs : SM.id Dom.Map.t;
      rng : Rng.t;
    }

    let rec reschedule n t =
      if n > 0 then
        let rng = Rng.next t.rng in
        let c = Dom.(Rng.value rng % of_int_exn (Map.length t.cs)) in
        match Map.find t.cs c with
        | None -> reschedule (n-1) {rng; cs = Map.remove t.cs c}
        | Some id -> SM.status id >>= function
          | `Dead -> reschedule (n-1) {rng; cs = Map.remove t.cs c}
          | _ -> SM.switch id >>| fun () -> {t with rng}
      else
        SM.forks () >>| fun fs -> {
          t with
          cs = Seq.foldi fs ~init:Dom.Map.empty ~f:(fun i cs id ->
              Map.add cs ~key:(Dom.of_int_exn i) ~data:id)
        }

    let schedule t = reschedule attempts t

  end

  module Fast = Make(Int)(RNG.LCG)
end
