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


  let inspect_instance (name,key,locations) =
    Sexp.List (Sexp.Atom name ::
               sexp_of_key key ::
               List.map locations ~f:sexp_of_key)

  let location,report_location =
    Primus.Observation.provide "incident-location"
      ~desc:"Occurs when the location of a possible incident is created."
      ~inspect:(fun (key,trace) ->
          Sexp.List [sexp_of_key key; sexp_of_trace trace])

  let new_instance,report_new_instance =
    Primus.Observation.provide "incident-new-instance"
      ~inspect:inspect_instance
      ~desc:"Occurs on a new instance of the incident class"

  let new_class,report_new_class =
    Primus.Observation.provide "incident-new-class"
      ~inspect:inspect_instance
      ~desc:"Occures when a new incident class is created"

  let new_representative,report_new_representative =
    Primus.Observation.provide "incident-new-representative"
      ~inspect:inspect_instance
      ~desc:"Occurs when a new representative of the class is selected"

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

  let set_of_trace {trace} = Set.of_list (module Bitvec_order) trace

  let last_blk = function
    | _ :: blk :: _ -> blk
    | [blk] -> blk
    | [] -> failwith "last_blk: empty trace"

  let is_member cls traces =
    List.for_all2_exn traces cls.points ~f:(fun {trace} points ->
        Set.mem points (last_blk trace))

  let set_cls name id cls s = {
    s with incidents = Map.update s.incidents name ~f:(function
      | None -> Primus.Value.Map.singleton id cls
      | Some classes -> Map.set classes id cls)
  }


  let classify {incidents; locations} name traces
      ~new_representative
      ~new_instance
      ~new_class =
    let traces = List.filter_map ~f:(Map.find locations) traces in
    let points = List.map ~f:set_of_trace traces in
    let cls' = {points; traces} in
    match Map.find incidents name with
    | None -> new_class cls'
    | Some classes ->
      Map.fold classes ~init:None ~f:(fun ~key ~data:cls found ->
          match found with
          | Some _ -> found
          | None ->
            if is_member cls traces
            then Some (new_instance key)
            else if is_member cls' cls.traces
            then Some (new_representative key cls')
            else None) |> function
      | None -> new_class cls'
      | Some other -> other

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
    open Location

    module Value = Primus.Value.Make(Machine)
    let (!!) = Machine.Observation.make

    let next name incidents = match Map.find incidents name with
      | None -> Value.of_int 1 ~width:63
      | Some classes -> match Map.max_elt classes with
        | None -> Value.of_int 1 ~width:63
        | Some (k,_) -> Value.succ k


    [@@@warning "-P"]
    let run (name :: locations) =
      Value.Symbol.of_value name >>= fun name ->
      !!report (name,locations) >>= fun () ->
      Machine.Global.get Location.state >>= fun s ->
      Location.classify s name locations
        ~new_representative:(fun id cls ->
            !!report_new_instance (name,id,locations) >>= fun () ->
            !!report_new_representative (name,id,locations) >>= fun () ->
            Machine.Global.update Location.state
              (Location.set_cls name id cls))
        ~new_instance:(fun id ->
            !!report_new_instance (name,id,locations))
        ~new_class:(fun cls ->
            next name s.incidents >>= fun id ->
            !!report_new_class (name,id,locations) >>= fun () ->
            !!report_new_instance (name,id,locations) >>= fun () ->
            Machine.Global.update Location.state
              (Location.set_cls name id cls))
      >>= fun () ->
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
