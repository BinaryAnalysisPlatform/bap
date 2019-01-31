open Core_kernel
open Or_error


let to_int conv sexp v = match conv v with
  | Some v -> Ok v
  | None -> error "doesn't fit into int" v sexp


let int_of_int64 = to_int Int64.to_int Int64.sexp_of_t
let int_of_int32 = to_int Int32.to_int Int32.sexp_of_t
let int_of_nativeint = to_int Nativeint.to_int Nativeint.sexp_of_t
let int_of_word = Bap_bitvector.to_int
