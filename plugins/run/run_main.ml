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
    `P "Run a program in the Primus emulator. ";
  ];;

  let argv = param (array string)  "argv"
      ~doc:"Process argument vector"

  let envp = param (array string) "env"
      ~doc:"Program environemt as a comma separated list of VAR=VAL pairs";;

  let entry = param (list string) "entry-points"
      ~doc:

        "Can be a list of entry points or a special keyword
      $(b,all-subroutines). An entry point is either a string denoting
      a function name, a tid starting with the $(b,%) percent, or an
      address in a hexadecimal format prefixed with $(b,0x).
      When the option is specified, the Primus Machine will start the
      execution from the specified entry point(s). Otherwise the
      execution will be started from all program terms that are marked
      with the [entry_point] attribute. If there are several entry
      points, then the execution will be started from all of them in
      parallel, i.e., by forking the machine and starting each machine
      from its own entry point. Consider enabling corresponding
      scheduler. If neither the argument nor there any entry points in
      the program, then a function called $(b,_start) is called. If
      $(b,all-subroutines) are specified then Primus will execute all
      subroutines in topological order"

  let in_isolation = flag "in-isolation"
      ~doc:"Run each entry point as an isolated machine.

      Each entry point is started in a separate machine and run
      sequentially, with the project data structure passed between them."


  let with_repetitions = flag "with-repetitions"
      ~doc:

        "The pass runs subroutines in the topological order meaning
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

let all_subroutines prog =
  let entries = entry_points prog in
  let start = Tid.create () in
  let non_entry =
    let roots = Tid.Set.of_list (start :: entries) in
    fun t -> if Set.mem roots t then None else Some (`tid t) in
  List.map entries ~f:(fun t -> `tid t) @
  Seq.to_list @@
  Seq.filter_map ~f:non_entry @@
  Graphlib.reverse_postorder_traverse (module Graphs.Callgraph)
    ~start @@
  callgraph start prog

let parse_entry_points proj entry = match entry with
  | ["all-subroutines"] -> all_subroutines (Project.program proj)
  | [] -> List.map (entry_points (Project.program proj)) ~f:(fun x ->
      `tid x)
  | xs -> List.map ~f:(name_of_entry (Project.arch proj)) xs

let parse_entry_points proj entry =
  match parse_entry_points proj entry with
  | [] -> [`symbol "_start"]
  | xs -> xs

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

let run_all need_repeat envp args proj state xs =
  Primus.Machine.run ~envp ~args proj state @@
  run need_repeat xs

let is_visited proj = function
  | `addr _ | `symbol _ -> false
  | `tid t ->
    match Term.find sub_t (Project.program proj) t with
    | None -> true
    | Some sub -> match Term.first blk_t sub with
      | None -> true
      | Some blk -> Term.has_attr blk Term.visited


let run_sep need_repeat envp args proj state xs =
  let total = List.length xs in
  let init = Primus.Normal,proj,state in
  Result.return @@
  List.foldi xs ~init ~f:(fun stage (status,proj,state) p ->
      report_progress ~stage ~total ();
      if not need_repeat && is_visited proj p then (status,proj,state)
      else match Primus.Machine.run ~envp ~args proj state @@ begin
          exec p >>=
          fun () -> Eval.halt >>=
          never_returns
        end with Error err ->
        info "exec %a finished with a conflict: %a"
          Primus.Linker.Name.pp p
          Knowledge.Conflict.pp err;
        info "discarding the result and proceeding to the next entry";
        (status,proj,state)
               | Ok s -> s)

let main {Config.get=(!)} proj =
  let open Param in
  let state = Toplevel.current () in
  let run = if !in_isolation
    then run_sep !with_repetitions
    else run_all !with_repetitions in
  parse_entry_points proj !entry |> run !envp !argv proj state |> function
  | Ok (Primus.Normal,proj,state)
  | Ok (Primus.Exn Primus.Interpreter.Halt,proj,state) ->
    Toplevel.set state;
    info "Ok, we've terminated normally";
    proj
  | Ok (Primus.Exn exn,proj,state) ->
    Toplevel.set state;
    info "program terminated by a signal: %s" (Primus.Exn.to_string exn);
    proj
  | Error conflict ->
    info "The computation was terminated because of a conflict: %a"
      Knowledge.Conflict.pp conflict;
    proj

let deps = [
  "trivial-condition-form"
]

let () =
  Config.when_ready (fun conf ->
      Project.register_pass ~deps (main conf);
      Primus.Machine.add_component (module Visited) [@warning "-D"];
      Primus.Components.register_generic "records-visited" (module Visited)
        ~package:"primus-run-internal")
