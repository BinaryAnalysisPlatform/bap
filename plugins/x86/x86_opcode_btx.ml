open Core_kernel.Std

(** BT *)
type bt_rr_ia32 = [
  | `BT32rr
  | `BT16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_rr = [
  | `BT64rr
  | bt_rr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_ri_ia32 = [
  | `BT32ri8
  | `BT16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_ri = [
  | `BT64ri8
  | bt_ri_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_mr_ia32 = [
  | `BT32mr
  | `BT16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_mr = [
  | `BT64mr
  | bt_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_mi_ia32 = [
  | `BT32mi8
  | `BT16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_mi = [
  | `BT64mi8
  | bt_mi_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_ia32 = [
  | bt_rr_ia32
  | bt_ri_ia32
  | bt_mi_ia32
  | bt_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bt = [
  | bt_rr
  | bt_ri
  | bt_mi
  | bt_mr
] [@@deriving bin_io, sexp, compare, enumerate]

(** BTC *)
type btc_rr_ia32 = [
  | `BTC32rr
  | `BTC16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_rr = [
  | `BTC64rr
  | btc_rr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_ri_ia32 = [
  | `BTC32ri8
  | `BTC16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_ri = [
  | `BTC64ri8
  | btc_ri_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_mr_ia32 = [
  | `BTC32mr
  | `BTC16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_mr = [
  | `BTC64mr
  | btc_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_mi_ia32 = [
  | `BTC32mi8
  | `BTC16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_mi = [
  | `BTC64mi8
  | btc_mi_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_ia32 = [
  | btc_rr_ia32
  | btc_ri_ia32
  | btc_mi_ia32
  | btc_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btc = [
  | btc_rr
  | btc_ri
  | btc_mi
  | btc_mr
] [@@deriving bin_io, sexp, compare, enumerate]

(** BTR *)
type btr_rr_ia32 = [
  | `BTR32rr
  | `BTR16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_rr = [
  | `BTR64rr
  | btr_rr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_ri_ia32 = [
  | `BTR32ri8
  | `BTR16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_ri = [
  | `BTR64ri8
  | btr_ri_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_mr_ia32 = [
  | `BTR32mr
  | `BTR16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_mr = [
  | `BTR64mr
  | btr_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_mi_ia32 = [
  | `BTR32mi8
  | `BTR16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_mi = [
  | `BTR64mi8
  | btr_mi_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_ia32 = [
  | btr_rr_ia32
  | btr_ri_ia32
  | btr_mi_ia32
  | btr_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btr = [
  | btr_rr
  | btr_ri
  | btr_mi
  | btr_mr
] [@@deriving bin_io, sexp, compare, enumerate]

(** BTS *)
type bts_rr_ia32 = [
  | `BTS32rr
  | `BTS16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_rr = [
  | `BTS64rr
  | bts_rr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_ri_ia32 = [
  | `BTS32ri8
  | `BTS16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_ri = [
  | `BTS64ri8
  | bts_ri_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_mr_ia32 = [
  | `BTS32mr
  | `BTS16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_mr = [
  | `BTS64mr
  | bts_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_mi_ia32 = [
  | `BTS32mi8
  | `BTS16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_mi = [
  | `BTS64mi8
  | bts_mi_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_ia32 = [
  | bts_rr_ia32
  | bts_ri_ia32
  | bts_mi_ia32
  | bts_mr_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type bts = [
  | bts_rr
  | bts_ri
  | bts_mi
  | bts_mr
] [@@deriving bin_io, sexp, compare, enumerate]

(** btx_rr, btx_ri, btx_mi, btx_mr *)
type btx_rr = [
  | bt_rr
  | btc_rr
  | btr_rr
  | bts_rr
]  [@@deriving bin_io, sexp, compare, enumerate]

type btx_ri = [
  | bt_ri
  | btc_ri
  | btr_ri
  | bts_ri
]  [@@deriving bin_io, sexp, compare, enumerate]

type btx_mi = [
  | bt_mi
  | btc_mi
  | btr_mi
  | bts_mi
]  [@@deriving bin_io, sexp, compare, enumerate]

type btx_mr = [
  | bt_mr
  | btc_mr
  | btr_mr
  | bts_mr
]  [@@deriving bin_io, sexp, compare, enumerate]

(** btx_ia32, btx_amd64 *)

type btx_ia32 = [
  | bt_ia32
  | btc_ia32
  | btr_ia32
  | bts_ia32
] [@@deriving bin_io, sexp, compare, enumerate]

type btx = [
  | btx_ia32
  | bt
  | btc
  | btr
  | bts
] [@@deriving bin_io, sexp, compare, enumerate]
