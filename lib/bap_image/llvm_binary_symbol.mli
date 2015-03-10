(** LLVM symbol *)

type t

type kind =
  | Unknown
  | Data
  | Debug
  | File
  | Function
  | Other

val name : t -> string
val kind : t -> kind
val addr : t -> int64
val size : t -> int64
