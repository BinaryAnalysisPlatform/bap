open Core_kernel.Std

type info = string * string with bin_io, compare, sexp
type t = info list with bin_io, compare, sexp

let (++) xs x = x :: xs
let info ~name ~data = (name,data)
let find = List.Assoc.find ~equal:String.equal
let to_list = ident
