open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus.Std
open Bap_future.Std
include Self()

module Id = Monad.State.Multi.Id
module Name = Tid

module Cfg = struct
  open Config

  let max_length = param (some int) "max-length"
      ~doc:
        "Limits the maximum number of basic blocks a single machinine
      can execute"

  let max_visited = param (some int) "max-visited"
      ~doc:
        "Limits the maximum number of executions of the same block in
      a given machine"

end

let get p = Future.peek_exn (Config.determined p)


type state = {
  visited : int Name.Map.t;
  length : int;
}

let state = Primus.Machine.State.declare
              ~name:"primus-limiter"
              ~uuid:"b1b58dff-54de-4611-abf8-88ff8f6c7481"
              (fun _ -> {
                   visited = Name.Map.empty;
                   length = 0;
                 })


module Main(Machine : Primus.Machine.S) = struct
  open Machine.Syntax


  module Eval = Primus.Interpreter.Make(Machine)

  let count_call call =
    Machine.Local.update state ~f:(fun s -> {
          s with
          length = s.length + 1;
        })

  let terminate reason =
    Machine.current () >>= fun id ->
    info
      "terminating machine %a because a maximum number of %s has been reached"
      Id.pp id reason;
    Eval.halt >>=
    never_returns

  let check_max_length length = match get Cfg.max_length with
    | Some max_length when length > max_length ->
      terminate "blocks "
    | _ -> Machine.return ()

  let check_max_visits name visited = match get Cfg.max_visited with
    | None -> Machine.return ()
    | Some max_visited -> match Map.find visited name with
    | Some visits when visits > max_visited ->
      terminate "visits of the same destination"
    | _ -> Machine.return ()

  let check_call call =
    Machine.Local.get state >>= fun {length} ->
    check_max_length length

  let on_exec call =
    count_call call >>= fun () ->
    check_call call

  let on_blk blk =
    let name = Term.tid blk in
    Machine.Local.get state >>= fun s ->
    check_max_visits name s.visited >>= fun () ->
    Machine.Local.put state {
      s with visited = Map.update s.visited name ~f:(function
        | None -> 1
        | Some n -> n + 1)
    }

  let init () = Machine.sequence [
      Primus.Linker.exec >>> on_exec;
      Primus.Interpreter.enter_blk >>> on_blk
    ]

end


let () = Config.when_ready (fun _ ->
    Primus.Machine.add_component (module Main))
