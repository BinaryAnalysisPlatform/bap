open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus.Std
include Self()

type t = {
  pending : Primus.Machine.id Fqueue.t
}

let state = Primus.Machine.State.declare
              ~uuid:"d1b33e16-bf5d-48d5-a174-3901dff3d123"
              ~name:"round-robin-scheduler"
              (fun _ -> {pending=Fqueue.empty})


module RR(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let rec schedule t = match Fqueue.dequeue t.pending with
    | None -> Machine.forks () >>= fun fs -> schedule {
        pending = Seq.fold fs ~init:Fqueue.empty ~f:Fqueue.enqueue
      }
    | Some (next,pending) -> Machine.status next >>= function
      | `Dead -> schedule {pending}
      | _ -> Machine.switch next >>| fun () -> {pending}

  let step _blk =
    Machine.Local.get state >>=
    schedule >>=
    Machine.Local.put state

  let init () =
    Primus.Interpreter.leave_blk >>> step
end

let enable () =
  info "enabling the scheduler";
  Primus.Machine.add_component (module RR)

open Config;;
manpage [
  `S "DESCRIPTION";

  `P
    "The round-robin scheduler will try to distribute machine time
    equally between competing clones. The state tree will be traversed
    in an order that is close to the bread-first search order";

  `P
    "The round-robin scheduler will switch the context after each basic block."
];;

let enabled = flag "scheduler" ~doc:"Enable the scheduler."


let () = when_ready (fun {get=(!!)} -> if !!enabled then enable ())
