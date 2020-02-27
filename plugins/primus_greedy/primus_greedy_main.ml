open Core_kernel
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Format
include Self()

(* continue with the same context, until a path terminates,
   then switch to a next thread that is not yet terminated.
*)

module Id = Monad.State.Multi.Id
module Ids = Id.Set

type state = {
  halted : Ids.t;
}


let state = Primus.Machine.State.declare
    ~uuid:"328fd42b-1ffd-44da-8400-8494732dcfa3"
    ~name:"greedy-scheduler-state"
    (fun _ -> {halted = Ids.empty})



module Greedy(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Eval = Primus.Interpreter.Make(Machine)

  let pp_halted ppf halted =
    Set.iter halted ~f:(fun id ->
        fprintf ppf "%a" Machine.Id.pp id)

  let last = Seq.fold ~init:None ~f:(fun _ x -> Some x)

  let reschedule () =
    Machine.Global.get state >>= fun {halted} ->
    Machine.forks () >>= fun forks ->
    let active = Seq.filter forks ~f:(fun id -> not (Set.mem halted id)) in
    let total = Seq.length active + Set.length halted in
    let stage = Set.length halted - 1 in
    report_progress ~stage ~total ();
    match last active with
    | None ->
      info "no more pending machines";
      Machine.switch Machine.global
    | Some cid ->
      Machine.current () >>= fun pid ->
      info "switch to machine %a from %a" Id.pp cid Id.pp pid;
      info "killing previous machine %a" Id.pp pid;
      Machine.kill pid >>= fun () ->
      Machine.switch cid


  let halt () =
    Machine.current () >>= fun pid ->
    Machine.Global.update state ~f:(fun {halted} ->
        {halted = Set.add halted pid}) >>= fun () ->
    reschedule ()


  let init () =
    Primus.Machine.finished >>> halt
end

let enable () =
  info "enabling the scheduler";
  Primus.Components.register_generic "greedy-scheduler" (module Greedy)

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
    "The greedy scheduler will attempt to reschedule every time a basic
    block is evaluated."
];;

let enabled = flag "scheduler" ~doc:"Enable the scheduler."


let () = when_ready (fun {get=(!!)} -> if !!enabled then enable ())
