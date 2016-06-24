open Core_kernel.Std

type decls = (string * Bap_c_type.t) list
type parser = Bap_c_size.base -> string -> decls Or_error.t

let parser = ref None
let provide p = parser := Some p

let run size file = match !parser with
  | None -> Or_error.error_string "C parser is not available"
  | Some parse -> parse size file
