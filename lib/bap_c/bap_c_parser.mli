open Core_kernel.Std

val run : string -> (string * Bap_c_type.t) list Or_error.t

val provide : (string -> (string * Bap_c_type.t) list Or_error.t) -> unit
