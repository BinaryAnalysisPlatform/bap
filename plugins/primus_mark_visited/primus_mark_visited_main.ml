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


let mark_visited t = Term.set_attr t Term.visited ()

let marker visited = object
  inherit Term.mapper as super
  method! map_blk t =
    if Set.mem visited (Term.tid t) then
      mark_visited t |>
      Term.map def_t ~f:mark_visited |>
      Term.map jmp_t ~f:mark_visited
    else t
end


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



  let mark () =
    Machine.Global.get state >>= fun {visited} ->
    Machine.update (fun proj ->
        let marker = marker visited in
        Project.with_program proj @@
        marker#run (Project.program proj))

  let init () =
    Machine.sequence [
      Primus.Interpreter.enter_blk >>> visit;
      Primus.Machine.finished >>> mark;
      Primus.Linker.Trace.lisp_call >>> visit_stub;
    ]
end



open Config;;
manpage [
  `S "DESCRIPTION";
  `P
    "Marks all terms visited by any Primus machine with the
     [Term.visited] attribute. Terms will not be marked during the
     execution, but only after a machine finishes."
]

let () = when_ready (fun _ ->
    Primus.Machine.add_component (module Main) [@warning "-D"];
    Primus.Components.register_generic "mark-visited" (module Main)
      ~package:"primus"
      ~desc:"marks visited program terms with the [visited] attribute")
