open Core_kernel
open Regular_data_types
open Regular_data_intf


module Make(T : Versioned) : Data with type t := T.t

val sexp_reader : (module Sexpable with type t = 'a) -> 'a reader
val sexp_writer : (module Sexpable with type t = 'a) -> 'a writer

val bin_reader : (module Binable with type t = 'a) -> 'a reader
val bin_writer : (module Binable with type t = 'a) -> 'a writer

val marshal_reader : (module T with type t = 'a) -> 'a reader
val marshal_writer : (module T with type t = 'a) -> 'a writer
val pretty_writer : (module Pretty_printer.S with type t = 'a) -> 'a writer
