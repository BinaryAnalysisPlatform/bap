open Core_kernel
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Format

include Self()

type id = Primus.Machine.id
module Id = Monad.State.Multi.Id

module Location = struct
  type point = {
    mach : id;
    addr : Addr.t;
  }


  type trace = {
    backtrace : point list
  }
  type t = trace


  type state = {
    incidents : trace Primus.Value.Map.t;
  }


  let sexp_of_point {mach; addr} =
    Sexp.Atom (asprintf "%a:%s"
                 Id.pp mach
                 (Addr.string_of_value ~hex:true addr))

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

  let push {addr; mach} = function
    | [] -> [{mach; addr}]
    | x::_ as trace ->
      if Addr.equal x.addr addr then trace
      else {mach; addr}::trace

  module Record(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Eval = Primus.Interpreter.Make(Machine)

    let update_backtrace ~f =
      Machine.Local.update trace ~f:(fun {backtrace} ->
          {backtrace = f backtrace})

    let record_callsite _ =
      Eval.pc >>= fun addr ->
      Machine.current () >>= fun mach ->
      update_backtrace  ~f:(push {addr; mach})

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
      Eval.pc >>= fun addr ->
      Machine.current () >>= fun mach ->
      Machine.Local.get trace >>= fun {backtrace} ->
      Machine.Global.get state >>= fun {incidents} ->
      next incidents >>= fun key ->
      let trace = {backtrace = push {addr; mach} backtrace} in
      Machine.Global.put state {
        incidents = Map.set incidents ~key ~data:trace
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
