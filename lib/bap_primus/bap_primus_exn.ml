open Core_kernel[@@warning "-D"]

type t = exn = ..
let to_string err = Caml.Printexc.to_string err
let add_printer pr = Caml.Printexc.register_printer pr
