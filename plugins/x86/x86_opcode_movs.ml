open Core_kernel.Std

type movs = [
  | `MOVSB
  | `MOVSW
  | `MOVSD
  | `MOVSQ
] [@@deriving bin_io, sexp, compare, enumerate]
