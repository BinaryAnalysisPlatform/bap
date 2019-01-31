open Core_kernel

type cmps = [
  | `CMPS8
  | `CMPS16
  | `CMPS32
  | `CMPS64
] [@@deriving bin_io, sexp, compare, enumerate]

