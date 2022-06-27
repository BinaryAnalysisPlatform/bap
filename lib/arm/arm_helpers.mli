open Core_kernel[@@warning "-D"]

val sexpable_of_string : (Sexp.t -> 'a) -> string -> 'a option
