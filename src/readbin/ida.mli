(** IDA integration.

    This module provides an experimental integration with IDA,
    just as a proof of concept. It runs IDA in a batch mode, to
    obtain database, then runs a script on database that extract
    symbol

*)
open Bap.Std
open Core_kernel.Std

(** IDA instance  *)
type t

(** [create ?ida target] create an IDA instance that will work with
     [target] executable. [ida] is an optional hint, that can be
     either a full path to [ida] executable, or just an executable
     name *)
val create : ?ida:string -> string -> t Or_error.t

(** [get_symbols ?demangle ida mem] extract symbols from binary, using IDA *)
val get_symbols : ?demangle:Options.demangle -> t -> mem -> string table

(** [close ida] finish interaction with IDA and clean all resources  *)
val close : t -> unit


(** [with_file ?ida target analysis] creates ida instance on [target],
    perform [analysis] and close [ida] *)
val with_file : ?ida:string -> string -> (t -> 'a) -> 'a Or_error.t
