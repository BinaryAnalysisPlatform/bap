open Core_kernel
open Bap_primus_lisp_types

let read_exn s = Type (int_of_string (String.strip s))

let read s = Option.try_with (fun () -> read_exn s)

include Comparable.Make(struct
    type t = typ [@@deriving sexp, compare]
end)
