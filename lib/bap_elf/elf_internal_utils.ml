open Core_kernel.Std

let int_of_int64 n = match Int64.to_int n with
  | Some v -> Ok v
  | None -> Or_error.errorf "number %LdL doesn't fit into int" n
