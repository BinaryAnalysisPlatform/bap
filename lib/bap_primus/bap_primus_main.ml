open Core_kernel.Std
open Bap.Std
open Bap_primus_types
open Format

module Observation = Bap_primus_observation


let finished,finish =
  Observation.provide ~inspect:sexp_of_unit "fini"

let init,inited =
  Observation.provide ~inspect:sexp_of_unit "init"


let components : component list ref = ref []
let add_component comp = components := comp :: !components


module Main(Machine : Machine) = struct
  open Machine.Syntax

  module Mach = Bap_primus_machine
  module Link = Bap_primus_interpreter.Init(Machine)

  let init_components () =
    Machine.List.iter !components ~f:(fun (module Component) ->
        let module Comp = Component(Machine) in
        Comp.init ())

  let init () =
    Machine.Observation.make inited ()

  let finish () =
    Machine.Observation.make finish ()


  let run ?(envp=[| |]) ?(args=[| |]) proj m =
    let comp =
      init_components () >>= fun () ->
      init () >>= fun () ->
      Link.run () >>= fun () ->
      Machine.catch m (fun err ->
          finish () >>= fun () ->
          Machine.raise err)
      >>= fun x ->
      finish () >>= fun () ->
      Machine.return x in
    Machine.run comp proj args envp
end
