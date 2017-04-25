open Core_kernel.Std
open Bap.Std


let passes : (project -> project) list ref  = ref []

let pass proj =
  List.fold !passes ~init:proj ~f:(fun proj pass ->
      pass proj)

let register_pass pass =
  passes := pass :: !passes


let name = Value.Tag.register (module String)
    ~uuid:"ce10e129-4cae-4f49-9b21-8e00d3635067"
    ~name:"abi-name"
