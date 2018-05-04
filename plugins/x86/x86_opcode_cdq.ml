open Core_kernel.Std

type cdq = [
  | `CBW
  | `CWD
  | `CWDE
  | `CDQ
  | `CDQE
  | `CQO
  ] [@@deriving bin_io, sexp, compare, enumerate]