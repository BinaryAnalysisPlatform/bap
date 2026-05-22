open Core_kernel[@@warning "-D"]

type t = exn = ..
let to_string err = Stdlib.Printexc.to_string err
let add_printer pr = Stdlib.Printexc.register_printer pr
