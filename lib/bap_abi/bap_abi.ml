open Core_kernel.Std
open Bap.Std


let passes : (project -> project) list ref  = ref []

let pass proj =
  List.fold !passes ~init:proj ~f:(fun proj pass ->
      pass proj)

let register_pass pass =
  passes := pass :: !passes
