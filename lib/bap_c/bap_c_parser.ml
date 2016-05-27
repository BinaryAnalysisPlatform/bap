open Core_kernel.Std

let parser = ref None
let provide p = parser := Some p

let run file = match !parser with
  | None -> Or_error.error_string "C parser is not available"
  | Some parse -> parse file
