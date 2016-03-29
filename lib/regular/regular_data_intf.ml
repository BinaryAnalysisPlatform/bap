open Core_kernel.Std
open Regular_data_types

type bytes = Regular_bytes.t [@@deriving bin_io, compare, sexp]
type digest = string [@@deriving bin_io, compare, sexp]

type 'a reader = 'a Regular_data_read.t
type 'a writer = 'a Regular_data_write.t

module type Data = sig
  type t
  type info = string * [`Ver of string] * string option
  val version : string
  val size_in_bytes : ?ver:string -> ?fmt:string -> t -> int
  val of_bytes : ?ver:string -> ?fmt:string -> bytes -> t
  val to_bytes : ?ver:string -> ?fmt:string -> t -> bytes
  val blit_to_bytes : ?ver:string -> ?fmt:string -> bytes -> t -> int -> unit
  val of_bigstring : ?ver:string -> ?fmt:string -> bigstring -> t
  val to_bigstring : ?ver:string -> ?fmt:string -> t -> bigstring
  val blit_to_bigstring : ?ver:string -> ?fmt:string -> bigstring -> t -> int -> unit
  module Io : sig
    val read  : ?ver:string -> ?fmt:string -> string -> t
    val load  : ?ver:string -> ?fmt:string -> in_channel -> t
    val load_all : ?ver:string -> ?fmt:string -> ?rev:bool -> in_channel -> t list
    val scan  : ?ver:string -> ?fmt:string -> in_channel -> (unit -> t option)
    val write : ?ver:string -> ?fmt:string -> string -> t -> unit
    val save  : ?ver:string -> ?fmt:string -> out_channel -> t -> unit
    val save_all : ?ver:string -> ?fmt:string -> out_channel -> t list -> unit
    val dump  : ?ver:string -> ?fmt:string -> out_channel -> (unit -> t option) -> unit
    val show  : ?ver:string -> ?fmt:string -> t -> unit
    val print : ?ver:string -> ?fmt:string -> Format.formatter -> t -> unit
  end
  module Cache : sig
    val load : digest -> t option
    val save : digest -> t -> unit
  end
  val add_reader : ?desc:string -> ver:string -> string -> t reader -> unit
  val add_writer : ?desc:string -> ver:string -> string -> t writer -> unit
  val available_readers : unit -> info list
  val default_reader : unit -> info
  val set_default_reader : ?ver:string -> string -> unit
  val with_reader : ?ver:string -> string -> (unit -> 'a) -> 'a
  val available_writers : unit -> info list
  val default_writer : unit -> info
  val set_default_writer : ?ver:string -> string -> unit
  val with_writer : ?ver:string -> string -> (unit -> 'a) -> 'a
  val default_printer : unit -> info option
  val set_default_printer : ?ver:string -> string -> unit
  val with_printer : ?ver:string -> string -> (unit -> 'a) -> 'a
  val find_reader : ?ver:string -> string -> t reader option
  val find_writer : ?ver:string -> string -> t writer option
end


type 'a data = (module Data with type t = 'a)
