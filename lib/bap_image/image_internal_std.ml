open Core_kernel.Std
open Bap_types.Std

open Image_common

module Table = Bap_table
type 'a table = 'a Table.t with sexp_of

module Backend = Image_backend
type backend = Backend.t

module Memory = Bap_memory
type mem = Memory.t with sexp_of
