open Core_kernel.Std

type cmpxchg_rr_ia32 = [
  | `CMPXCHG8rr
  | `CMPXCHG16rr
  | `CMPXCHG32rr
] [@@deriving bin_io, sexp, compare, enumerate]

type cmpxchg_rr = [
  | cmpxchg_rr_ia32
  | `CMPXCHG64rr
] [@@deriving bin_io, sexp, compare, enumerate]

type cmpxchg_rm_ia32 = [
  | `CMPXCHG8rm
  | `CMPXCHG16rm
  | `CMPXCHG32rm
] [@@deriving bin_io, sexp, compare, enumerate]

type cmpxchg_rm = [
  | cmpxchg_rm_ia32
  | `CMPXCHG64rm
] [@@deriving bin_io, sexp, compare, enumerate]

type cmpxchg_ia32 = [
  | cmpxchg_rr_ia32
  | cmpxchg_rm_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type cmpxchg = [
  | cmpxchg_rr
  | cmpxchg_rm
] [@@deriving bin_io, sexp, compare, enumerate]
