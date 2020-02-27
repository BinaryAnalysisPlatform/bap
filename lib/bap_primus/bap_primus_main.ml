open Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_primus_types
open Format


module System = Bap_primus_system

let components = ref 0

let add_component component =
  incr components;
  let name = sprintf "unnamed-component-%d" !components in
  System.Components.register_generic name component
    ~desc:"an unnamed user component"

module Main(Machine : Machine) = struct
  open Machine.Syntax
  module Lisp = Bap_primus_lisp.Make(Machine)
  module Sys = System.Generic(Machine)

  let run ?(envp=[| |]) ?(args=[| |]) proj user =
    let comp =
      Sys.init System.default >>= fun () ->
      Lisp.typecheck >>= fun () ->
      Lisp.optimize () >>= fun () ->
      Machine.catch user (fun exn ->
          Machine.Observation.make System.inited () >>= fun () ->
          Machine.Observation.make System.finish () >>= fun () ->
          Machine.raise exn) >>= fun () ->
      Machine.Observation.make System.finish () in
    Machine.run comp proj args envp
end
