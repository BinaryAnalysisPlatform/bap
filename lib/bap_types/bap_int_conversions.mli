open Core_kernel

val int_of_int32     : int32 -> int Or_error.t
val int_of_int64     : int64 -> int Or_error.t
val int_of_nativeint : nativeint -> int Or_error.t
val int_of_word      : Bap_bitvector.t -> int Or_error.t
