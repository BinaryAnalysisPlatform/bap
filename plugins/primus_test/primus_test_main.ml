open Core_kernel
open Bap.Std
open Bap_primus.Std
open Monads.Std
open Format

include Self()

type id = Primus.Machine.id
module Id = Monad.State.Multi.Id

module Location = struct
  type point = Bitvec.t

  (*

     Each incident is represented by its name and a tuple of
     locations. Each location is represented as a trace. It is not
     requred, in general, that all traces that denote locations of an
     incident should belong to the same path of execution (i.e.,
     traces could easily be disjoint). The last (the most recent)
     point of the trace is called the _endpoint_ or just the location,
     when it is obvious from the context that we mean only the
     endpoint of the trace.

     Incidents of the same name are partitioned into locality classes,
     so each incident belongs to one and only one class. We say that
     an incident [i] subsumes an incident [j] iff the endpoint of each
     location in [j] is is in the trace of a corresponding location in
     [i].

     Two incidents belong to the same class, if one the incidents
     subsumes another. Of the set of incidents that belong to the same
     class, an incident that subsumes all other incidents is selected
     as a representative of the class. Note, that there could be more
     than one incidents, as they may have equal locations, which
     differ only in machine identifiers, which are not considered in
     the substring relation.


     When an incident is reported it either belongs to an existing
     class or starts its own class.

     When an incident is an instance of an existing class <cls>, then
     we make an observation:
     [(incident-new-instance <name> <cls> <p1> ... <pM>)]

     When an incident is not a member of any class a new class is
     created and an observation is made:

     [(incident-new-class <name> <cls> <p1> ... <pM>)].

     When a newly discovered instance of a class is selected as a new
     representative, in addition to the new-instance observation  we
     also make an observation

     [(incident-new-representative <name> <cls> <p1> ... <pM>)]
  *)

  type id = {id : string}

  type trace = {
    trace : point list;
    switches : (int * id) list;
    initial : id;
    length : int;
  }
  type t = trace

  type locations = {
    points : Set.M(Bitvec_order).t list;
    traces : trace list;
  }

  type state = {
    locations : trace Primus.Value.Map.t;
    incidents : locations Primus.Value.Map.t String.Map.t;
  }

  let sexp_of_point {id} addr =
    Sexp.Atom (asprintf "%s:%s" id @@
               Z.format "%x" (Bitvec.to_bigint addr))

  let prefixed_trace prefix t =
    let rec build id pos switches trace acc = match switches with
      | (p,newid) :: switches when p = pos ->
        build newid pos switches trace acc
      | _ -> match trace with
        | [] -> acc
        | p::ps -> build id (pos+1) switches ps @@
          prefix id p :: acc in
    build t.initial 0 t.switches (List.rev t.trace) []

  let sexp_of_trace t = Sexp.List (prefixed_trace sexp_of_point t)

  let sexp_of_key v =
    Sexp.Atom (asprintf "%a" Word.pp_dec (Primus.Value.to_word v))

  let location,report_location =
    Primus.Observation.provide "incident-location"
      ~desc:"Occurs when the location of a possible incident is created."
      ~inspect:(fun (key,trace) ->
          Sexp.List [sexp_of_key key; sexp_of_trace trace])

  let trace = Primus.Machine.State.declare
      ~uuid:"3663b645-87a6-4561-b75f-c447cdc221bc"
      ~name:"program-trace" @@ fun _ -> {
      trace = [];
      switches = [];
      initial = {id="unknown"};
      length = 0;
    }

  let state = Primus.Machine.State.declare
      ~uuid:"22d66ae9-82ad-4185-a1f8-5eb99f5569df"
      ~name:"incident-locations" @@ fun _ -> {
      incidents = String.Map.empty;
      locations = Primus.Value.Map.empty;
    }

  let push pc machine t =
    let pc = Word.to_bitvec pc
    and machine = Id.to_string machine in
    match t.trace with
    | [] -> {
        trace = [pc];
        initial = {id=machine};
        switches = [];
        length = 1;
      }
    | _ ->
      let current = match t.switches with
        | [] -> t.initial.id
        | (_,{id})::_ -> id in
      let t = if Bitvec.equal (List.hd_exn t.trace) pc
        then t else {
          t with trace = pc :: t.trace; length = t.length + 1;
        } in
      if String.equal current machine then t else {
        t with
        trace = pc :: t.trace;
        switches = (t.length,{id=machine}) :: t.switches;
        length = t.length + 1
      }

  module Record(Machine : Primus.Machine.S) = struct
    open Machine.Syntax
    module Eval = Primus.Interpreter.Make(Machine)

    let update_backtrace ~f =
      Machine.Local.update trace ~f

    let record_callsite _ =
      Eval.pc >>= fun addr ->
      Machine.current () >>= fun mach ->
      update_backtrace  ~f:(push addr mach)

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
      Machine.Local.get trace >>= fun t ->
      Machine.Global.get state >>= fun s ->
      next s.locations >>= fun key ->
      let t = push addr mach t in
      Machine.Global.put state {
        s with
        locations = Map.set s.locations ~key ~data:t
      } >>= fun () ->
      Machine.Observation.post report_location ~f:(fun report ->
          report (key,t)) >>| fun () ->
      key
  end

  module Create(Machine : Primus.Machine.S) = struct
    module Location = Make(Machine)
    let run _ = Location.record
  end
end

module Incident = struct
  let inspect (name,locs) =
    Sexp.List (Sexp.Atom name :: List.map ~f:Location.sexp_of_key locs)

  let t,report =
    Primus.Observation.provide ~inspect "incident"
      ~desc:"Occurs when the incident is reported that \
             involves the given locations."

  module Report(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    module Value = Primus.Value.Make(Machine)

    [@@@warning "-P"]
    let run (name :: locations) =
      Machine.Observation.post report ~f:(fun report ->
          Value.Symbol.of_value name >>= fun name ->
          report (name,locations)) >>= fun () ->
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
      def "incident-location" (unit @-> sym) (module Location.Create);
      def "incident-report" (one sym // all sym @-> bool) (module Incident.Report)
    ]
end

let main () =
  Primus.Machine.add_component (module Location.Record) [@warning "-D"];
  Primus.Machine.add_component (module Lisp) [@warning "-D"];
  Primus.Components.register_generic
    ~package:"bap" "incident-location-recorder"
    (module Location.Record)
    ~desc:"Records tracepoints for incident reporting.";
  Primus.Components.register_generic
    ~package:"bap" "lisp-incidents"
    (module Lisp)
    ~desc:"Exposes the incident reporting facitilites to Primus Lisp."

;;

open Config

;;
manpage [
  `S "DESCRIPTION";
  `P "Primus Testing and Program Verification module.";
];;


when_ready (fun _ -> main ())
