open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus.Std
open Bap_future.Std
include Self()

module Id = Monad.State.Multi.Id
module Name = Tid

type counter =
  | Blk
  | Insn
  | Term
  | Exp

type bound = {
  counter : counter;
  limit : int;
}

module Bound = struct
  let parse_int s =
    try Ok (Int.of_string s) with _ ->
      Error ("Expected a number, got " ^ s)

  let counter_of_suffix s =
    match String.nget s (String.length s - 1) with
    | 'b' -> Some Blk
    | 'i' -> Some Insn
    | 't' -> Some Term
    | 'e' -> Some Exp
    | _ -> None

  let suffix_of_counter = function
    | Blk -> "b"
    | Insn -> "i"
    | Term -> "t"
    | Exp -> ""

  let make_bound counter s = match parse_int s with
    | Ok limit -> `Ok {counter; limit}
    | Error s -> `Error s

  let parse s = match counter_of_suffix s with
    | None -> make_bound Exp s
    | Some cnt -> make_bound cnt s

  let print ppf {limit; counter} =
    Format.fprintf ppf "%d%s" limit (suffix_of_counter counter)

  let converter = Config.converter parse print {limit=0; counter=Exp}
end

module Cfg = struct
  open Config

  let max_length = param (some Bound.converter) "max-length"
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

  let terminate reason =
    Machine.current () >>= fun id ->
    info
      "terminating machine %a because a maximum number of %s has been reached"
      Id.pp id reason;
    Eval.halt >>=
    never_returns


  let string_of_counter = function
    | Blk -> "blocks"
    | Insn -> "instructions"
    | Term -> "terms"
    | Exp -> "expressions"


  let check_bound _ = match get Cfg.max_length with
    | None -> Machine.return ()
    | Some {limit; counter} ->
      Machine.Local.get state >>= fun s ->
      report_progress ~task:"limit max path length" ~stage:s.length ~total:limit ();
      if s.length > limit
      then terminate (string_of_counter counter)
      else Machine.Local.put state {
          s with length = s.length + 1;
        }



  let check_max_visits name visited = match get Cfg.max_visited with
    | None -> Machine.return ()
    | Some max_visited -> match Map.find visited name with
      | Some visits ->
        report_progress ~task:"limit max visits"
          ~stage:visits ~total:max_visited ();
        if visits > max_visited
        then terminate
            (sprintf "visits of the %s destination" (Tid.name name))
        else Machine.return ()
      | _ -> Machine.return ()

  let on_blk blk =
    let name = Term.tid blk in
    Machine.Local.get state >>= fun s ->
    check_max_visits name s.visited >>= fun () ->
    Machine.Local.put state {
      s with visited = Map.update s.visited name ~f:(function
        | None -> 1
        | Some n -> n + 1)
    }

  let register_counter =
    let open Primus.Interpreter in
    match get Cfg.max_length with
    | None -> Machine.return ()
    | Some {counter=Blk}  -> enter_blk >>> check_bound
    | Some {counter=Insn} -> pc_change >>> check_bound
    | Some {counter=Term} -> enter_term >>> check_bound
    | Some {counter=Exp}  -> Machine.sequence [
        loading >>> check_bound;
        storing >>> check_bound;
        binop >>> check_bound;
        unop >>> check_bound;
        Primus.Linker.exec >>> check_bound;
      ]

  let init () =
    Machine.sequence [
      Primus.Interpreter.enter_blk >>> on_blk;
      register_counter;
    ]

end


let () = Config.when_ready (fun _ ->
    Primus.Machine.add_component (module Main))
