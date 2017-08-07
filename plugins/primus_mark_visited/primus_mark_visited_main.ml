open Core_kernel.Std
open Bap.Std
open Monads.Std
open Bap_primus.Std
include Self()

type state = {
  visited : Tid.Set.t;
}

let state = Primus.Machine.State.declare
              ~name:"primus-mark-visitor"
              ~uuid:"6edf3c44-3665-4ec1-8537-ef7fbba78d3d"
              (fun _ -> {visited=Tid.Set.empty})


let marker = object
  inherit Term.mapper as super
  method! map_term cls t =
    Term.set_attr (super#map_term cls t) Term.visited ()

end

module Main(Machine : Primus.Machine.S) = struct
  open Machine.Syntax

  let visit t =
    Machine.Local.update state ~f:(fun {visited} -> {
          visited = Set.add visited t
        })

  let mark _ =
    Machine.Local.get state >>= fun {visited} ->
    Machine.update (fun proj ->
        Project.with_program proj @@
        marker#run (Project.program proj))

  let init () =
    Primus.Interpreter.enter_term >>> visit >>= fun () ->
    Primus.Interpreter.leave_blk  >>> mark
end



open Config;;
manpage [
  `S "DESCRIPTION";
  `P
    "Marks all terms visited by any Primus machine with the
     [Term.visited] attribute."
]

let () = when_ready (fun _ -> Primus.Machine.add_component (module Main))
