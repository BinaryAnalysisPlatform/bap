(** LEB128 - Little Endian Base 128 encoding. *)
open Core_kernel

(** an encoded value  *)
type t [@@deriving bin_io, compare, sexp]

(** [encode ~signed v] encodes value [v] in a LEB128 format. If
    signed is true, then uses signed encoding. *)
type 'a encoder = ?signed:bool -> 'a -> t
(** [decode leb] decodes a number from LEB128 representation.  *)
type 'a decoder = t -> 'a Or_error.t

(** [size leb] return size in bytes of the number stored in LEB128
    encoding.  *)
val size: t -> int
val read: ?signed:bool -> string -> pos_ref:int ref -> t Or_error.t
val write: t -> Bytes.t -> pos:int -> unit

val to_int:   int   decoder
val to_int32: int32 decoder
val to_int64: int64 decoder

val of_int:   int   encoder
val of_int32: int32 encoder
val of_int64: int64 encoder
