open Core_kernel
open Bap_types.Std

include Image_common

module Table = Bap_table
type 'a table = 'a Table.t [@@deriving sexp_of]

module Backend = Image_backend
type backend = Backend.t

module Memory = Bap_memory
type mem = Memory.t [@@deriving sexp_of]

module Memmap = Bap_memmap
type 'a memmap = 'a Memmap.t [@@deriving sexp_of]
