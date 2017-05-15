open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
include Self()

(* continue with the same context, until a path terminates,
   then switch to a next thread that is not yet terminated.

   A thread  is terminated, if [ctxt#next = None]
*)
module Greedy(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let schedule _ =
    Machine.get () >>= fun ctxt -> match ctxt#next with
    | Some _ -> Machine.return ()
    | None -> Machine.forks () >>= Machine.Seq.find ~f:(fun id ->
          Machine.switch id >>= fun () ->
          Machine.get () >>| fun ctxt -> match ctxt#next with
          | None -> false
          | Some _ ->
            info "switched to a machine clone #%s"
              (Machine.Id.to_string id);
            true) >>| ignore

  let init () =
    Primus.Interpreter.enter_blk >>> schedule
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
     another alive state that has a ($b,next) value that is not $(b,None) is
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
