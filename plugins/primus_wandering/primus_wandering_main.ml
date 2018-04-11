open Core_kernel
open Bap.Std
open Monads.Std
open Bap_future.Std
open Bap_primus.Std
include Self()



type t = {
  pending : Primus.Machine.id Int.Map.t;
}

let state = Primus.Machine.State.declare
    ~uuid:"99883d0e-94b2-41a4-bce6-1e4a949fd919"
    ~name:"wandering-scheduler"
    (fun _ -> {pending = Int.Map.empty})

module Make
    (Random : sig val generator : Primus.generator end)
    (Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Generate = Primus.Generator.Make(Machine)


  (** a number of attempts to pick a live state before the next
      draw.  *)
  let attempts = 4

  type 'a m = 'a Machine.t

  let drop i t =
    {pending = Map.remove t.pending i}


  (** [reschedule n s] performs [n] attempts to choose a live clone,
      from the contestants. If nothing was chosen, then draws a new
      party of contestants and starts again.

      The algorithm is guaranteed to terminate since it is guaranteed
      that there is at least one live state, called the global state.

      Note, a winner - the clone that was chosen, is removed from the
      draw.*)
  let rec reschedule n t =
    if n > 0 && not (Map.is_empty t.pending) then
      let (max,_) = Map.max_elt_exn t.pending in
      Generate.next Random.generator >>= fun i ->
      let i = i mod (max + 1) in
      match Map.find t.pending i with
      | None -> reschedule (n-1) t
      | Some id -> Machine.status id >>= function
        | `Dead -> reschedule (n-1) (drop i t)
        | _ -> Machine.switch id >>| fun () -> drop i t
    else
      Machine.forks () >>| fun fs -> {
        pending = Seq.foldi fs ~init:Int.Map.empty ~f:(fun i cs id ->
            Map.set cs ~key:i ~data:id);
      }

  let schedule t = reschedule attempts t

  let step _blk =
    Machine.Local.get state >>=
    schedule >>=
    Machine.Local.put state

  let init () =
    Primus.Interpreter.leave_pos >>> step

end

let enable seed =
  info "enabling the scheduler, using %d as the seed" seed;
  let module Random = struct
    let generator = Primus.Generator.Random.lcg seed
  end in
  let module Scheduler = Make(Random) in
  Primus.Machine.add_component (module Scheduler)

open Config;;
manpage [
  `S "DESCRIPTION";

  `P
    "The random wandering scheduler will pick between the states randomly.";

  `P
    "The round-robin scheduler will switch the context after each basic block."
];;

let enabled = flag "scheduler" ~doc:"Enable the scheduler."
let seed = param ~doc:"random generator seed" int "seed"



let () = when_ready (fun {get=(!!)} -> if !!enabled then enable !!seed)
