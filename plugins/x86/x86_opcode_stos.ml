open Core_kernel[@@warning "-D"]

type stos = [
  | `STOSB
  | `STOSW
  | `STOSD
  | `STOSQ
] [@@deriving bin_io, sexp, compare, enumerate]
