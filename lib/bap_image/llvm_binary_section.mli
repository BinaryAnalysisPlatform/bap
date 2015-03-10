(** LLVM section *)

type t
val name : t -> string
val addr : t -> int64
val size : t -> int64
