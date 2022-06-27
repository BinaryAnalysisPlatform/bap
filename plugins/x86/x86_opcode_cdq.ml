open Core_kernel[@@warning "-D"]

type cdq = [
  | `CBW
  | `CWD
  | `CWDE
  | `CDQ
  | `CDQE
  | `CQO
] [@@deriving bin_io, sexp, compare, enumerate]
