open Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_primus.Std
open Graphlib.Std
open Monads.Std
include Self()
open Format

module Callgraph = Graphs.Callgraph

module Param = struct
  open Config;;

  manpage [
    `S "DESCRIPTION";
    `P "Populates and run the Primus Jobs queue.
      Creates new job for each specified system and runs the queued
    jobs (including those that were already added to the queue by
    other analyses). After the job queue is emptied, commits the
    obtained knowledge and passes the resulting project data structure
    downstream.";
  ];;

  let argv = param (array string)  "argv"
      ~doc:"Process argument vector"

  let envp = param (array string) "env"
      ~doc:"Program environemt as a comma separated list of VAR=VAL pairs";;

  let entry = param (list string) "entry-points"
      ~doc:"Can be a list of entry points or a special keyword
      $(b,all-subroutines) or $(b,marked-subroutines). An entry point
      is either a string denoting a function name, a tid starting with
      the $(b,%) percent, or an address in a hexadecimal format
      prefixed with $(b,0x).  When the option is specified, the Primus
      Machine will start the execution from the specified entry
      point(s). Otherwise the execution will be started from all
      program terms that are marked with the [entry_point]
      attribute. If there are several entry points, then the execution
      will be started from all of them in parallel, i.e., by forking
      the machine and starting each machine from its own entry
      point. Consider enabling corresponding scheduler. If neither the
      argument nor there any entry points in the program, then a
      function called $(b,_start) is called. If $(b,all-subroutines)
      are specified then Primus will execute all subroutines in
      the topological order. If $(b,marked-subroutines) is specified,
      then Primus will execute the specified systems on all
      subroutines that has the $(b,mark) attribute."


  let in_isolation = flag "in-isolation"
      ~doc:"Run each entry point as new system.

      Each entry point is enqueued as a job and run in a separate
      systems. The project and knowledge is passed between each
      system, the rest of the state is discarded."


  let with_repetitions = flag "with-repetitions"
      ~doc:"The pass runs subroutines in the topological order meaning
      the farther a subroutine is in a callgraph from the roots the
      later it will be run as an entry point and higher chances it
      will be called before that from some other subroutine. And
      being a callee is more interesting case for analysis then
      being an entry point due to the wider context of the former.
      Given that, we skip by default such of entry points that
      were visited during the run of other ones.
      And this option disables this behavior and runs all the
      subroutines in a row. "
end


let pp_id = Monad.State.Multi.Id.pp

module Machine = Primus.Analysis
open Machine.Syntax

module Eval = Primus.Interpreter.Make(Machine)
module Linker = Primus.Linker.Make(Machine)
module Env = Primus.Env.Make(Machine)
module Lisp = Primus.Lisp.Make(Machine)

let string_of_name = function
  | `symbol s -> s
  | `tid t -> Tid.to_string t
  | `addr x -> Addr.string_of_value x

let name_of_entry arch entry =
  let width = Arch.addr_size arch |> Size.in_bits in
  if String.is_empty entry
  then invalid_arg "An entry point should be a non-empty string"
  else match entry.[0] with
    | '%' -> `tid (Tid.from_string_exn entry)
    | '0' -> `addr (Addr.of_int64 ~width (Int64.of_string entry))
    | _ -> `symbol entry

let entry_point_collector = object
  inherit [tid list] Term.visitor
  method! enter_term _ t entries =
    if Term.has_attr t Sub.entry_point
    then Term.tid t :: entries
    else entries
end

let entry_points prog =
  entry_point_collector#run prog []

let callgraph start prog =
  let mark_as_root n g =
    if Tid.(n = start) then g
    else Callgraph.Edge.(insert (create start n []) g) in
  let connect_inputs g =
    Callgraph.nodes g |>
    Seq.fold ~init:g ~f:(fun g n ->
        if Callgraph.Node.degree ~dir:`In n g = 0
        then mark_as_root n g
        else g) in
  let connect_unreachable_scc g =
    Graphlib.depth_first_search (module Callgraph) g
      ~start
      ~init:g
      ~start_tree:mark_as_root in
  Program.to_graph prog |>
  connect_inputs |>
  connect_unreachable_scc

let all_subroutines ?(marked=false) prog =
  let entries = entry_points prog in
  let start = Tid.create () in
  let roots = Tid.Set.of_list (start :: entries) in
  let unmarked = if not marked then Tid.Set.empty else
      Term.enum sub_t prog |>
      Seq.fold ~init:Tid.Set.empty ~f:(fun unmarked sub ->
          if Term.has_attr sub mark then unmarked
          else Set.add unmarked (Term.tid sub)) in
  let marked_non_root_to_entry t =
    if Set.mem roots t || Set.mem unmarked t then None
    else Some (`tid t) in
  List.filter_map entries ~f:(fun t ->
      if Set.mem unmarked t then None else Some (`tid t)) @
  Seq.to_list @@
  Seq.filter_map ~f:marked_non_root_to_entry @@
  Graphlib.reverse_postorder_traverse (module Graphs.Callgraph)
    ~start @@
  callgraph start prog

let parse_entry_points proj entry =
  let prog = Project.program proj in
  match entry with
  | ["all-subroutines"] -> all_subroutines prog
  | ["marked-subroutines"] -> all_subroutines ~marked:true prog
  | [] -> List.(entry_points prog >>| fun x -> `tid x)
  | xs -> List.(xs >>| name_of_entry (Project.arch proj))

let parse_entry_points proj entries =
  match entries,parse_entry_points proj entries with
  | ["marked-subroutines"],[] -> []
  | _,[] -> [`symbol "_start"]
  | _,xs -> xs

let exec x =
  Machine.current () >>= fun cid ->
  info "Fork %a: starting from the %s entry point"
    pp_id cid (string_of_name x);
  Machine.catch (Linker.exec x)
    (fun exn ->
       info "execution from %s terminated with: %s "
         (string_of_name x)
         (Primus.Exn.to_string exn);
       Machine.return ())

let visited = Primus.Machine.State.declare
    ~uuid:"c6028425-a8c7-48cf-b6d9-57a44ed9a08a"
    ~name:"visited-subroutines"
    (fun _ -> Set.empty (module Tid))

module Visited(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let visit sub =
    Machine.Global.update visited ~f:(fun tids -> Set.add tids (Term.tid sub))

  let init () = Primus.Interpreter.enter_sub >>> visit
end


module Id = Monad.State.Multi.Id

let is_visited = function
  | `tid tid ->
    Machine.Global.get visited >>= fun subs ->
    Machine.return (Set.mem subs tid)
  | _ -> Machine.return false

let run need_repeat entries =
  let total = List.length entries in
  let rec loop stage = function
    | [] ->
      info "all toplevel machines done, halting";
      Eval.halt >>=
      never_returns
    | x :: xs ->
      Machine.current () >>= fun pid ->
      Machine.fork ()    >>= fun () ->
      Machine.current () >>= fun cid ->
      if Id.(pid = cid)
      then loop (stage+1) xs
      else
        is_visited x >>= fun is_visited ->
        if is_visited && not need_repeat then
          Eval.halt >>=
          never_returns
        else
          exec x >>= fun () ->
          report_progress ~stage ~total ();
          Eval.halt >>=
          never_returns in
  loop 0 entries

let enqueue_super_job need_repeat envp args sys xs =
  Primus.Jobs.enqueue sys ~envp ~args
    ~name:"super-job"
    ~desc:"runs all entries in one system"
    ~start:(run need_repeat xs)

let is_visited proj = function
  | `addr _ | `symbol _ -> false
  | `tid t ->
    match Term.find sub_t (Project.program proj) t with
    | None -> true
    | Some sub -> match Term.first blk_t sub with
      | None -> true
      | Some blk -> Term.has_attr blk Term.visited

let enqueue_separate_jobs need_repeat envp args sys xs =
  let total = List.length xs in
  List.iteri xs ~f:(fun stage p ->
      Primus.Jobs.enqueue sys
        ~args ~envp
        ~name:(Primus.Linker.Name.to_string p)
        ~start:begin
          Machine.get () >>= fun proj ->
          report_progress ~stage ~total ();
          if not need_repeat && is_visited proj p
          then Machine.return ()
          else exec p
        end)

let on_success job _ _ : Primus.Jobs.action =
  info "job %s finished successfully" (Primus.Job.name job);
  Continue

let on_failure job conflict : Primus.Jobs.action =
  info "job %s failed to converge and exited with conflict: %a"
    (Primus.Job.name job) Knowledge.Conflict.pp conflict;
  Continue

let main {Config.get=(!)} proj =
  let open Param in
  let state = Toplevel.current () in
  let sys = Primus.System.Repository.get ~package:"primus" "legacy-main" in
  let enqueue_jobs = if !in_isolation
    then enqueue_separate_jobs else enqueue_super_job in
  let inputs = parse_entry_points proj !entry in
  enqueue_jobs !with_repetitions !envp !argv sys inputs;
  let result = Primus.Jobs.run ~on_failure ~on_success proj state in
  Toplevel.set (Primus.Jobs.knowledge result);
  Primus.Jobs.project result

let deps = [
  "trivial-condition-form"
]

let () =
  Config.when_ready (fun conf ->
      Project.register_pass ~deps (main conf);
      Primus.Machine.add_component (module Visited) [@warning "-D"];
      Primus.Components.register_generic "records-visited" (module Visited)
        ~package:"primus-run-internal")
