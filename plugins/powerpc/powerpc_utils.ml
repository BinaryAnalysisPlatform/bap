open Core_kernel.Std
open Bap.Std

let ppc_fail format =
  let fail str = failwith (sprintf "PowerPC lifter fail: %s" str) in
  Printf.ksprintf fail format
