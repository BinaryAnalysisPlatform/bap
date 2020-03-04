open Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_primus_types
open Format


module System = Bap_primus_system

let components = ref 0
let system = ref @@ System.define "legacy-main"
    ~package:"primus"
    ~components:[
      System.component ~package:"primus" "binary-program";
    ]
    ~desc:"The legacy Primus system that contains
all components registered with the Machine.add_component
function."

let add_component component =
  let package = "primus-internal" in
  incr components;
  let name = sprintf "legacy-main-component-%d" !components in
  system := System.add_component !system ~package name;
  System.Components.register_generic ~package name component
    ~desc:"an anonymous component registered via the legacy interface"

module Main(Machine : Machine) = struct
  module System = System.Generic(Machine)
  let run ?envp ?args proj user =
    System.run ?envp ?args ~start:user !system proj
end

let run ?envp ?args proj state user =
  System.run ?envp ?args ~start:user !system proj state
