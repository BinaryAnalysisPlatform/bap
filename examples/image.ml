open Core_kernel.Std
open Bap.Std

let img = match Image.create "/bin/ls" with
  | Ok (img, []) -> img
  | _ -> assert false

let () =
  let secnum = Table.length (Image.sections img) in
  assert (secnum <> 0)
