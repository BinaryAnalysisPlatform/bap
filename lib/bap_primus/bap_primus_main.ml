open Core_kernel
open Bap_knowledge
open Bap.Std
open Bap_primus_types
open Format

let package = "bap"


module System = Bap_primus_system

let components = ref 0

let () = System.Repository.add @@ System.define "legacy-main"
    ~package
    ~components:[
      System.component ~package "load-binary";
    ]
    ~desc:"The legacy Primus system that contains \
           all components registered with the Machine.add_component \
           function."


let add_component component =
  incr components;
  let name = sprintf "legacy-main-component-%d" !components in
  System.Components.register_generic name component
    ~internal:true
    ~package:"bap-internal"
    ~desc:"an anonymous component of the legacy main system";
  System.Repository.update ~package "legacy-main" ~f:(fun system ->
      System.add_component system ~package:"bap-internal" name)

module Main(Machine : Machine) = struct
  include System.Generic(Machine)
  let run ?envp ?args proj user =
    let main = System.Repository.get ~package "legacy-main" in
    run ?envp ?args ~start:user main proj
end
