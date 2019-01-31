open Core_kernel

type scas = [
  | `SCAS8
  | `SCAS16
  | `SCAS32
  | `SCAS64
] [@@deriving bin_io, sexp, compare, enumerate]
