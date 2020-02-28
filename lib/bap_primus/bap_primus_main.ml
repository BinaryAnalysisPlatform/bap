open Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_primus_types
open Format


module System = Bap_primus_system
module Interpreter = Bap_primus_interpreter

let components : component list ref = ref []

let add_component component =
  components := component :: !components

module Main(Machine : Machine) = struct
  open Machine.Syntax
  module Lisp = Bap_primus_lisp.Make(Machine)
  module Link = Interpreter.LinkBinaryProgram(Machine)

  let init_components () =
    Machine.List.iter !components ~f:(fun (module Component) ->
        let module Comp = Component(Machine) in
        Comp.init ())

  let run ?(envp=[| |]) ?(args=[| |]) proj user =
    let comp =
      init_components () >>= fun () ->
      Link.init () >>= fun () ->
      Lisp.typecheck >>= fun () ->
      Lisp.optimize () >>= fun () ->
      Machine.catch user (fun exn ->
          Machine.Observation.make System.inited () >>= fun () ->
          Machine.Observation.make System.finish () >>= fun () ->
          Machine.raise exn) >>= fun () ->
      Machine.Observation.make System.finish () in
    Machine.run comp proj args envp
end
