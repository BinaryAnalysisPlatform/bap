open Core_kernel

type outs = [
  | `OUTSB
  | `OUTSW
  | `OUTSD
] [@@deriving bin_io, sexp, compare, enumerate]
