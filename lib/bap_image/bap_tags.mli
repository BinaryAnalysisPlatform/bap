open Core_kernel.Std

type info = string * string
with bin_io, compare, sexp

type t with bin_io, compare, sexp

val (++) : t -> info -> t

val info : name:string -> data:string -> info
val find : t -> string -> string option
val to_list : t -> info list
