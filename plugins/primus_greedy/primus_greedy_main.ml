open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Monads.Std
include Self()

(* continue with the same context, until a path terminates,
   then switch to a next thread that is not yet terminated.
*)

module Ids = Monad.State.Multi.Id.Set

type state = {
  halted : Ids.t;
}

let state = Primus.Machine.State.declare
    ~uuid:"328fd42b-1ffd-44da-8400-8494732dcfa3"
    ~name:"greedy-scheduler-state"
    (fun _ -> {halted = Ids.empty})


module Greedy(Machine : Primus.Machine.S) = struct
  open Machine.Syntax


  let reschedule () =
    Machine.current () >>= fun id -> 
    Machine.Local.get state >>= fun {halted} -> 
    let halted = Set.add halted id in
    Machine.Local.put state {halted} >>= fun () -> 
    Machine.forks () >>= fun forks ->
    Seq.find forks ~f:(fun id -> 
        not (Set.mem halted id)) |> function
    | None -> Machine.return ()
    | Some id ->
      info "switched to machine %a" Machine.Id.pp id;
      Machine.switch id

  let init () =
    Primus.Interpreter.halting >>> reschedule
end

let enable () =
  info "enabling the scheduler";
  Primus.Machine.add_component (module Greedy)

open Config;;
manpage [
  `S "DESCRIPTION";

  `P
    "The greedy scheduling strategy will continue with the same state,
     unless the machine reaches a termination state, i.e., when the
     $(b,next) value in a context becomes $(b,None). In that case
     another alive state that has a $(b,next) value that is not $(b,None) is
     chosen. If such state doesn't exist, then the Machine finally
     terminates. Thus this strategy will perform a depth-first
     traversal of the state tree, and guarantees that all paths
     are explored";

  `P
    "The greedy scheduler will attempt to reschedule everytime a basic
    block is evaluated."
];;

let enabled = flag "scheduler" ~doc:"Enable the scheduler."


let () = when_ready (fun {get=(!!)} -> if !!enabled then enable ())
