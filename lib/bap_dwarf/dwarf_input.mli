open Core_kernel.Std
open Bap_types.Std
open Dwarf_types

type 'a reader = string -> pos_ref : int ref -> 'a Or_error.t

val char: int reader
val code: int reader
val tag: tag reader
val form: form reader
val attr: attr reader
val string: string reader
val address: endian -> Word_size.t -> addr reader
val version: int reader
val unit_size: endian -> (Word_size.t * int) reader
val skip: bytes:int -> unit reader
val skip_zeros: unit reader
val address_size: Word_size.t reader
val offset: endian -> Word_size.t -> int reader

val block: lenspec -> endian -> string reader
val const: lenspec -> endian -> int64 reader

val map: 'a reader -> f:('a -> 'b) -> 'b reader
val take: 'a reader -> 'a option reader
val drop: 'a reader -> 'a option reader
val ignore: 'a reader -> unit reader
val pair: 'a reader -> 'b reader -> ('a * 'b) reader
