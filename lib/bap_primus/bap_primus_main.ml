open Core_kernel.Std
open Bap.Std
open Bap_primus_types
open Format

module Observation = Bap_primus_observation


let finished,finish =
  Observation.provide ~inspect:sexp_of_unit "machine-finished"


let components : component list ref = ref []
let add_component comp = components := comp :: !components


module Main(Machine : Machine) = struct
  open Machine.Syntax

  module Mach = Bap_primus_machine
  module Init = Bap_primus_interpreter.Init(Machine)

  let init_components () =
    eprintf "Initializing %d components@\n%!" (List.length !components);
    Machine.List.iter !components ~f:(fun (module Component) ->
        let module Comp = Component(Machine) in
        eprintf "Initializing a component@\n%!";
        Comp.init ())

  let run ?(envp=[| |]) ?(args=[| |]) proj m =
    let comp =
      eprintf "Starting the machine@\n";
      Init.run () >>= fun () ->
      init_components () >>= fun () -> 
      eprintf "Components are initialized@\n";
      eprintf "Starting computation@\n";
      m >>= fun x ->
      eprintf "Computation done, finishing@\n";
      Machine.Observation.make finish () >>= fun () ->
      Machine.return x in
    Machine.run comp proj args envp
end
