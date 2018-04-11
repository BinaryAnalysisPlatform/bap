open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus.Std
include Self()


type t = {
  explored : int Tid.Map.t;
  pending : Primus.Machine.id Fqueue.t
}

let state = Primus.Machine.State.declare
    ~uuid:"5a863fc2-96cf-4a00-b046-b9b38f95aa11"
    ~name:"exploring-scheduler"
    (fun _ -> {
         pending=Fqueue.empty;
         explored=Tid.Map.empty;

       })
module Scheduler(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Eval = Primus.Interpreter.Make(Machine)

  (** [enqueue level state pid] adds [pid] to the list of ids that
      will be explored, if the [next|pid] points to an unvisited term, or
      to a term that was visited the number of times equal to
      [level].
      As a side effect will add an unvisited term pointed by [next] (if
      any) to the [explored] mapping, with the number of explorations
      equal to zero. Also, will leave the machine the state with the
      given [pid].
  *)
  let enqueue level t id  =
    let add ?(level=0) tid = {
      pending = Fqueue.enqueue t.pending id;
      explored = Map.set t.explored ~key:tid ~data:level
    } in
    Machine.switch id >>= fun () ->
    Eval.pos >>| Primus.Pos.tid >>| fun tid ->
    match Map.find t.explored tid with
    | None -> add tid
    | Some 0 -> t
    | Some n when n = level -> add ~level tid
    | Some _ -> t

  let remove_planned_explorations t = {
    t with explored = Map.filteri t.explored
               ~f:(fun ~key ~data -> data <> 0)
  }

  let rec reschedule level t =
    Machine.current () >>= fun id ->
    Machine.forks () >>=
    Machine.Seq.fold ~init:t ~f:(enqueue level) >>= fun t ->
    Machine.switch id >>= fun () ->
    if Fqueue.is_empty t.pending then reschedule (level+1) t
    else Machine.return @@ remove_planned_explorations t

  let rec schedule t =
    match Fqueue.dequeue t.pending with
    | None -> reschedule 0 t >>= schedule
    | Some (id,pending) ->
      Machine.switch id >>| fun () -> {t with pending}

  let visit t =
    Machine.Global.update state ~f:(fun s -> {
          s with
          explored = Map.update s.explored (Term.tid t)
              ~f:(function
                  | None -> 1
                  | Some n -> n+1)
        })


  let step _blk =
    Machine.Global.get state >>=
    schedule >>=
    Machine.Global.put state

  let init () =
    Machine.List.sequence [
      Primus.Interpreter.leave_blk >>> step;
      Primus.Interpreter.leave_blk >>> visit;
      Primus.Interpreter.leave_sub >>> visit;
    ]
end

let enable () =
  info "enabling the scheduler";
  Primus.Machine.add_component (module Scheduler)

open Config;;
manpage [
  `S "DESCRIPTION";

  `P
    "The exploring scheduler will prioritize clones that will wonder
    into not yet explored or less explored areas. More specifically,
    from a set of machine clones, it will choose those, that will
    proceed to a basic block that was visited the least amount of times";

  `P
    "The round-robin scheduler will switch the context after each
    basic block. It will count the number of time the block was evaluated."
];;

let enabled = flag "scheduler" ~doc:"Enable the scheduler."


let () = when_ready (fun {get=(!!)} -> if !!enabled then enable ())
