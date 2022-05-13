open Core_kernel[@@warning "-D"]
open Bap.Std

let mips_fail format =
  let fail str = failwith (sprintf "MIPS lifter fail: %s" str) in
  Printf.ksprintf fail format
