open Core_kernel[@@warning "-D"]

type movs = [
  | `MOVSB
  | `MOVSW
  | `MOVSD
  | `MOVSQ
] [@@deriving bin_io, sexp, compare, enumerate]
