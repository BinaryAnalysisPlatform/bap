open Core_kernel
open Bap.Std
open Monads.Std
open Bap_primus.Std
include Self()

type state = {
  visited : Tid.Set.t;
  total  : int;
}

let count_blks p = (object
  inherit [int] Term.visitor
  method! enter_blk _ bs = bs + 1
end)#run p 0


let state = Primus.Machine.State.declare
    ~name:"primus-mark-visitor"
    ~uuid:"6edf3c44-3665-4ec1-8537-ef7fbba78d3d"
    (fun p -> {
         visited = Tid.Set.empty;
         total = count_blks (Project.program p)
       })

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
    if not@@p (Term.tid t) then t
    else mark_block marker t
end

let always _ = true
let is_mem xs x = Set.mem xs x

module Main(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  module Linker = Primus.Linker.Make(Machine)

  let visit t =
    Machine.Global.update state ~f:(fun s ->
        let s = {
          s with
          visited = Set.add s.visited (Term.tid t)
        } in
        report_progress ~stage:(Set.length s.visited - 1) ~total:s.total ();
        s)

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
        let marker = marker always dead in
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



open Config;;
manpage [
  `S "DESCRIPTION";
  `P
    "Marks all terms visited by any Primus machine with the
     [Term.visited] attribute and terms that weren't visited.
     with [Term.dead]. Terms will not be marked visited during the
     execution, but only after a system finishes."
]

let () = when_ready (fun _ ->
    Primus.Machine.add_component (module Main) [@warning "-D"];
    Primus.Components.register_generic "mark-visited" (module Main)
      ~package:"primus"
      ~desc:"marks visited program terms with the [visited] attribute")
