open Core_kernel
open Regular_data_types
open Regular_data_intf

type 'a cls
type info = string * [`Ver of string] * string option

module type Instance = sig
  include Versioned
  val instance : t cls
end

module type With_instance = sig
  include Instance
  include Data with type t := t
end

module New(T : Versioned) : Instance with type t = T.t
module Make(T : Versioned) : With_instance with type t := T.t
module Extend( T : Instance) : With_instance with type t := T.t

val sexp_reader : (module Sexpable with type t = 'a) -> 'a reader
val sexp_writer : (module Sexpable with type t = 'a) -> 'a writer

val bin_reader : (module Binable with type t = 'a) -> 'a reader
val bin_writer : (module Binable with type t = 'a) -> 'a writer

val marshal_reader : (module T with type t = 'a) -> 'a reader
val marshal_writer : (module T with type t = 'a) -> 'a writer
val pretty_writer : (module Pretty_printer.S with type t = 'a) -> 'a writer

val all_readers : unit -> (string * info list) list
val all_writers : unit -> (string * info list) list
