type ispec = [
  | `any
  | `unk
  | `special
  | `tag of string
  | `asm of string
  | `insn of string * string
]


val init :
  enable_intrinsics:ispec list ->
  with_fp:bool -> unit -> unit
