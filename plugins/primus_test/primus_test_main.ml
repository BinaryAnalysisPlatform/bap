open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
open Format

include Self()

module Location = struct
  type point = Addr.t


  type trace = {
    backtrace : point list
  }
  type t = trace


  type state = {
    incidents : trace Primus.Value.Map.t;
  }


  let sexp_of_point p =
    Sexp.Atom (Addr.string_of_value ~hex:true p)

  let sexp_of_t {backtrace} =
    Sexp.List (List.map ~f:sexp_of_point backtrace)

  let sexp_of_id v =
    Sexp.Atom (asprintf "%a" Word.pp_dec (Primus.Value.to_word v))

  let inspect (key,trace) =
    Sexp.List [sexp_of_id key; sexp_of_t trace]

  let location,report_location =
    Primus.Observation.provide ~inspect "incident-location"

  let trace = Primus.Machine.State.declare
      ~uuid:"41ed6b21-5ebd-4853-9797-30caff63c9ce"
      ~name:"program-trace"
      (fun _ -> {backtrace = []})

  let state = Primus.Machine.State.declare
      ~uuid:"24bb6d34-089e-4c17-81da-e71001600531"
      ~name:"incident-locations"
      (fun _ -> {incidents = Primus.Value.Map.empty})

  module Record(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Eval = Primus.Interpreter.Make(Machine)

    let update_backtrace ~f =
      Machine.Local.update trace ~f:(fun {backtrace} ->
          {backtrace = f backtrace})

    let record_callsite _ =
      Eval.pc >>= fun pc ->
      update_backtrace  ~f:(function
          | [] -> [pc]
          | x::_ as trace ->
            if Addr.equal x pc then trace else pc::trace)

    let init () =
      Primus.Interpreter.leave_blk >>> record_callsite
  end

  module Make(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Eval = Primus.Interpreter.Make(Machine)
    module Value = Primus.Value.Make(Machine)

    let next incidents = match Map.max_elt incidents with
      | None -> Value.of_int 1 ~width:63
      | Some (k,_) -> Value.succ k

    let record =
      Eval.pc >>= fun pc ->
      Machine.Local.get trace >>= fun {backtrace} ->
      Machine.Global.get state >>= fun {incidents} ->
      next incidents >>= fun key ->
      let trace = {backtrace = pc :: backtrace} in
      Machine.Global.put state {
        incidents = Map.add incidents ~key ~data:trace
      } >>= fun () ->
      Machine.Observation.make report_location (key,trace) >>| fun () ->
      key
  end

  module Create(Machine : Primus.Machine.S) = struct
    module Location = Make(Machine)
    let run _ = Location.record
  end
end

module Incident = struct
  let inspect (name,locs) =
    Sexp.List (Sexp.Atom name :: List.map ~f:Location.sexp_of_id locs)

  let t,report =
    Primus.Observation.provide ~inspect "incident"

  module Report(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Value = Primus.Value.Make(Machine)

    [@@@warning "-P"]
    let run (name :: locations) =
      Value.Symbol.of_value name >>= fun name ->
      Machine.Observation.make report (name,locations) >>= fun () ->
      Value.b1

  end
end

module Lisp(Machine : Primus.Machine.S) = struct
  module Lisp = Primus.Lisp.Make(Machine)
  open Primus.Lisp.Type.Spec

  let def name types closure =
    Lisp.define ~types name closure

  let init () =
    Machine.sequence [
      def "incident-location" (unit @-> a) (module Location.Create);
      def "incident-report" (one sym // all a @-> b) (module Incident.Report)
    ]
end

let main () =
  Primus.Machine.add_component (module Location.Record);
  Primus.Machine.add_component (module Lisp)

;;

open Config

;;
manpage [
  `S "DESCRIPTION";
  `P "Primus Testing and Program Verification module.";
];;


when_ready (fun _ -> main ())
