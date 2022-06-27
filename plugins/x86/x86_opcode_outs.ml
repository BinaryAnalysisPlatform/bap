open Core_kernel[@@warning "-D"]

type outs = [
  | `OUTSB
  | `OUTSW
  | `OUTSD
] [@@deriving bin_io, sexp, compare, enumerate]
