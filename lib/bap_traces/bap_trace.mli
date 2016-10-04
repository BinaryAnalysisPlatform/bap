open Core_kernel.Std
open Regular.Std
open Bap.Std

type event = value [@@deriving bin_io, sexp, compare]
type monitor
type proto
type tool [@@deriving bin_io, sexp]
type id
type t

type io_error = [
  | `Protocol_error of Error.t   (** Data encoding problem         *)
  | `System_error of Unix.error  (** System error                  *)
]

type error = [
  | io_error
  | `No_provider    (** No provider for a given URI               *)
  | `Ambiguous_uri  (** More than one provider for a given URI    *)
]
val load : ?monitor:monitor -> Uri.t -> (t,error) Result.t
val save : Uri.t -> t -> (unit,error) Result.t
val id : t -> id
val set_attr : t -> 'a tag -> 'a -> t
val get_attr : t -> 'a tag -> 'a option
val has_attr : t -> 'a tag -> bool
val tool : t -> tool
val meta : t -> dict
val set_meta : t -> dict -> t
val supports : t -> 'a tag -> bool
val read_all : t -> 'a tag -> 'a seq
val read_all_matching : t -> 'a Value.Match.t -> 'a seq
val read_events : t -> event seq
val next : t -> 'a tag -> 'a option
val next_event : t -> event option
val next_matching : t -> 'a Value.Match.t -> 'a option
val filter_map : t -> f:(event -> event option) -> t
val create : ?monitor:monitor -> tool -> (unit -> event Or_error.t option) -> t

module type S = sig
  val name: string
  val supports: 'a tag -> bool
end

module type P = sig
  include S
  val probe: Uri.t -> bool
end

val register_tool  : (module S) -> tool
val register_proto : (module P) -> proto

module Reader : sig
  type t = {
    tool : tool;                (** a tool descriptor read from trace *)
    meta : dict;                (** meta information read from trace  *)
    next : unit -> event Or_error.t option;     (** a stream function  *)
  }
end

type reader = Reader.t

val register_reader : proto -> (Uri.t -> id -> (reader, io_error) Result.t) -> unit

val register_writer : proto -> (Uri.t -> t -> (unit, io_error) Result.t) -> unit

module Id : Regular.S with type t = id

type step = [
  | `Stop
  | `Skip
  | `Fail
  | `Make of event
]
module Monitor : sig
  type t = monitor
  val ignore_errors : t
  val warn_on_error : (Error.t -> unit) -> t
  val fail_on_error : t
  val stop_on_error : t
  val pack_errors : (Error.t -> event) -> t
  val create : (Error.t -> step) -> t
end
