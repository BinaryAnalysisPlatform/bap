open Core_kernel
open Bap.Std
open Bap_primus.Std

type state = {
  visited : Tid.Set.t;
  total  : int;
}

let count_blks p = (object
  inherit [int] Term.visitor
  method! enter_blk _ bs = bs + 1
end)#run p 0


let init_visited prog =
  (object inherit [Tid.Set.t] Term.visitor
    method! enter_blk blk visited =
      if Term.has_attr blk Term.visited
      then Set.add visited (Term.tid blk)
      else visited
  end)#run prog Tid.Set.empty

let state = Primus.Machine.State.declare
    ~name:"primus-track-visited"
    ~uuid:"6edf3c44-3665-4ec1-8537-ef7fbba78d3d" @@ fun p -> {
    visited = init_visited (Project.program p);
    total = count_blks (Project.program p)
  }

type marker = { mark : 'a. 'a term -> 'a term}

let dead = {mark = fun t -> Term.set_attr t Term.dead ()}
let live = {mark = fun t ->
    Term.set_attr (Term.del_attr t Term.dead) Term.visited ()
  }

let mark_block {mark} t =
  mark t |>
  Term.map def_t ~f:mark |>
  Term.map jmp_t ~f:mark

let marker p marker = object
  inherit Term.mapper
  method! map_blk t =
    if not (p t) then t
    else mark_block marker t
end

let unvisited t = not (Term.has_attr t Term.visited)
let is_mem xs x = Set.mem xs (Term.tid x)

let progress,report_progress =
  Primus.Observation.provide "visited-blocks"
    ~inspect:[%sexp_of:int * int]
    ~desc:"visited-blocks (visited,total) is posted every time \
           a new basic block is visited, where [visited] is the \
           number of already visited blocks and [total] \
           is the total number of blocks in the program."

module Tracker(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Linker = Primus.Linker.Make(Machine)

  let visit t =
    Machine.Global.get state >>= fun s ->
    let s = {
      s with
      visited = Set.add s.visited (Term.tid t)
    } in
    Machine.Observation.post report_progress ~f:(fun k ->
        k (Set.length s.visited, s.total)) >>= fun () ->
    Machine.Global.put state s

  let visit_stub (name,_) =
    Linker.resolve_tid (`symbol name) >>= function
    | None -> Machine.return ()
    | Some tid -> Machine.gets Project.program >>= fun prog ->
      match Term.find sub_t prog tid with
      | None -> Machine.return ()
      | Some sub ->
        Term.enum blk_t sub |>
        Machine.Seq.iter ~f:visit

  let mark_live _ =
    Machine.Global.get state >>= fun {visited} ->
    Machine.update (fun proj ->
        let marker = marker (is_mem visited) live  in
        Project.with_program proj @@
        marker#run (Project.program proj))

  let mark_dead _ =
    Machine.update (fun proj ->
        let marker = marker unvisited dead in
        Project.with_program proj @@
        marker#run (Project.program proj))

  let init () =
    Machine.sequence [
      Primus.Interpreter.enter_blk >>> visit;
      Primus.System.stop >>> mark_live;
      Primus.Linker.Trace.lisp_call >>> visit_stub;
      Primus.System.start >>> mark_dead;
    ]
end

module Set = struct
  module Make(Machine : Primus.Machine.S) = struct
    open Machine.Syntax

    let all = Machine.Local.get state >>| fun s -> s.visited
    let mem t = all >>| fun s -> Set.mem s t

    let add t = Machine.Local.update state ~f:(fun s ->
        {s with visited = Set.add s.visited t})

    let del t = Machine.Local.update state ~f:(fun s ->
        {s with visited = Set.remove s.visited t})
  end
end

let init () =
  Primus.Components.register_generic "mark-visited" (module Tracker)
    ~package:"bap"
    ~desc:"Marks visited (by Primus) program terms with the \
           [Term.visited] attribute and unvisited with the \
           [Term.dead] attribute. Note, that the attributes \
           are attached only when the system exits";
  Primus.Machine.add_component (module Tracker) [@warning "-D"]
