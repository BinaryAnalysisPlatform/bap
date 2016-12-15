open Core_kernel.Std

type mov_rr_ia32 = [
  | `MOV8rr
  | `MOV8rr_NOREX
  | `MOV8rr_REV
  | `MOV16rr
  | `MOV16rr_REV
  | `MOV32rr
  | `MOV32rr_REV
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rr = [
  | mov_rr_ia32
  | `MOV64rr
  | `MOV64rr_REV
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ri_ia32 = [
  | `MOV8ri
  | `MOV16ri
  | `MOV32ri
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ri = [
  | mov_ri_ia32
  | `MOV64ri
  | `MOV64ri32
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mi_ia32 = [
  | `MOV8mi
  | `MOV16mi
  | `MOV32mi
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mi = [
  | mov_mi_ia32
  | `MOV64mi32
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rm_ia32 = [
  | `MOV8rm
  | `MOV8rm_NOREX
  | `MOV16rm
  | `MOV32rm
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rm = [
  | mov_rm_ia32
  | `MOV64rm
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mr_ia32 = [
  | `MOV8mr
  | `MOV8mr_NOREX
  | `MOV16mr
  | `MOV32mr
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_mr = [
  | mov_mr_ia32
  | `MOV64mr
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rs_ia32 = [
  | `MOV16rs
  | `MOV32rs
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_rs = [
  |  mov_rs_ia32
  | `MOV64rs
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ms_ia32 = [
  | `MOV16ms
  | `MOV32ms
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ms = [
  |  mov_ms_ia32
  | `MOV64ms
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_sr_ia32 = [
  | `MOV16sr
  | `MOV32sr
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_sr = [
  |  mov_sr_ia32
  | `MOV64sr
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_sm_ia32 = [
  | `MOV16sm
  | `MOV32sm
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_sm = [
  |  mov_sm_ia32
  | `MOV64sm
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_ia32 = [
  | mov_rr_ia32
  | mov_ri_ia32
  | mov_mi_ia32
  | mov_rm_ia32
  | mov_mr_ia32
  | mov_rs_ia32
  | mov_ms_ia32
  | mov_sr_ia32
  | mov_sm_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type mov_amd64 = [
  | mov_rr
  | mov_ri
  | mov_mi
  | mov_rm
  | mov_mr
  | mov_rs
  | mov_ms
  | mov_sr
  | mov_sm
] [@@deriving bin_io, sexp, compare, enumerate]

type mov = mov_amd64 [@@deriving bin_io, sexp, compare, enumerate]
