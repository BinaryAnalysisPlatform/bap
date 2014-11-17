open Core_kernel.Std


(** takes an OCaml code as a string and evaluates it for side effects *)
val eval : string -> bool Or_error.t
