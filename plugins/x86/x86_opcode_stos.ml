open Core_kernel.Std

type stos = [
  | `STOSB
  | `STOSW
  | `STOSD
  | `STOSQ
] [@@deriving bin_io, sexp, compare, enumerate]
