(** LLVM segment *)

type t
val name : t -> string
val offset : t -> int64
val addr : t -> int64
val size : t -> int64
val is_readable : t -> bool
val is_writable : t -> bool
val is_executable : t -> bool
