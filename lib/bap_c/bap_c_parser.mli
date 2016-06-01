(** A parser interface.

    The module doesn't provide any parsers by itself, but allows it to
    be provided by a third party module.
*)
open Core_kernel.Std

(** [run filename] parses file and returns a mapping from identifier
    to its type.*)
val run : string -> (string * Bap_c_type.t) list Or_error.t

(** called by a plugin that provides a parser.  *)
val provide : (string -> (string * Bap_c_type.t) list Or_error.t) -> unit
