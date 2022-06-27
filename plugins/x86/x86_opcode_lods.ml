open Core_kernel[@@warning "-D"]

type lods = [
  | `LODSB
  | `LODSW
  | `LODSD
  | `LODSQ
] [@@deriving bin_io, sexp, compare, enumerate]
