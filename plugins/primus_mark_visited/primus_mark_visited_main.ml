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


let marker visited = object
  inherit Term.mapper as super
  method! map_blk t =
    if Set.mem visited (Term.tid t) then
      Term.map def_t t ~f:(fun d -> Term.set_attr d Term.visited ())
    else t
end


module Main(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let visit t =
    Machine.Global.get state >>= fun {total; visited} ->
    report_progress ~stage:(Set.length visited) ~total ();
    Machine.Global.put state {
      total;
      visited = Set.add visited (Term.tid t)
    }


  let mark () =
    Machine.Global.get state >>= fun {visited} ->
    Machine.update (fun proj ->
        let marker = marker visited in
        Project.with_program proj @@
        marker#run (Project.program proj))

  let init () = Machine.sequence [
      Primus.Interpreter.enter_blk >>> visit;
      Primus.Machine.finished >>> mark;
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

let () = when_ready (fun _ -> Primus.Machine.add_component (module Main))
