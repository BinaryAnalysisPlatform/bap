(** A parser interface.

    The module doesn't provide any parsers by itself, but allows it to
    be provided by a third party module.
*)
open Core_kernel.Std

type decls = (string * Bap_c_type.t) list
type parser = Bap_c_size.base -> string -> decls Or_error.t

(** [run filename] parses file and returns a mapping from identifier
    to its type.*)
val run : parser

(** called by a plugin that provides a parser.  *)
val provide : parser -> unit
