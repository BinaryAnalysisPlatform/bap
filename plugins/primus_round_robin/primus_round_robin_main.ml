open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus.Std
open Format
include Self()

module Mid = Monad.State.Multi.Id

type t = {
  pending : Mid.t Fqueue.t;
  finished : Mid.Set.t
}

let state = Primus.Machine.State.declare
    ~uuid:"d1b33e16-bf5d-48d5-a174-3901dff3d123"
    ~name:"round-robin-scheduler"
    (fun _ -> {
         pending = Fqueue.empty;
         finished = Mid.Set.empty;
       })


module RR(Machine : Primus.Machine.S) = struct
  open Machine.Syntax



  let rec schedule t = match Fqueue.dequeue t.pending with
    | None ->
      Machine.forks () >>| Seq.filter ~f:(fun id ->
          not (Set.mem t.finished id)) >>= fun fs ->
      if Seq.is_empty fs
      then Machine.return ()
      else schedule {
          t with
          pending = Seq.fold fs ~init:Fqueue.empty ~f:Fqueue.enqueue
        }
    | Some (next,pending) ->
      Machine.status next >>= function
      | `Dead ->
        eprintf "Machine %a is dead, skipping over@\n%!"
          Machine.Id.pp next;
        schedule {pending; finished = Set.add t.finished next}
      | _ ->
        eprintf "Switching to machine %a@\n"
          Machine.Id.pp next;
        Machine.Global.put state {t with pending} >>= fun () ->
        Machine.switch next >>= fun () ->
        Machine.Global.get state >>= schedule

  let step _ =
    Machine.Global.get state >>= schedule

  let finish () =
    Machine.current () >>= fun id ->
    Machine.Global.update state ~f:(fun t ->
        {t with finished = Set.add t.finished id}) >>= fun () ->
    eprintf "machine %a is done@\n%!" Machine.Id.pp id;
    step ()


  let init () =
    Machine.sequence [
      Primus.Interpreter.leave_blk >>> step;
      Primus.System.fini >>> finish;
    ]
end

let register enabled =
  if enabled
  then Primus.Machine.add_component (module RR) [@warning "-D"];
  Primus.Components.register_generic "round-robin-scheduler" (module RR)
    ~package:"bap"
    ~desc:"Enables the round-robin scheduler (experimental)."

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


let () = when_ready (fun {get=(!!)} -> register !!enabled)
