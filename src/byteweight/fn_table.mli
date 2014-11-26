(** Function Table.
    Reads a function table from a binary file. *)
open Core_kernel.Std
open Bap.Std

(** a path in a filesystem  *)
type path = string

(** a probabiliry ranging from [0..1]  *)
type prob = float

(** function. See {!Fn} submodule for the interface.  *)
type fn


(** function table  *)
type t = fn table

(** [create ?signatures ?threshold ?work_on_bytes arch section] creates a
    function table from provided binary [data], assuming that it
    contains codes compiled for the [arch] architecture.

    @signatures - a path to a file, containing code signatures,
                        by default compiled in signatures are used.

    @weight_threshold - make a descisions with a specified confidence level
                        (at minimum)

    @work_on_bytes - if true, then do not lift code and work on a
                     binary. Defaults to false *)
val create
  : ?signatures:path             (* defaults to system signature *)
  -> ?weight_threshold:prob      (* defaults to zero *)
  -> ?work_on_bytes:bool         (* defaults to false     *)
  -> arch
  -> sections:Image.Sec.t table
  -> fn table

(** [fns table] returns a list of functions  *)
val fns : t -> fn Sequence.t

(** [addrs table] returns a sequence of addresses *)
val addrs : t -> addr Sequence.t

module Fn : sig
  type t = fn

  (** [name fn] is a [Some name] of a function [fn] if it is known.
      ByteWeight cannot come up with function name; but maybe it is still useful
      to keep the name field to keep consisted with dwarf function table *)
  val name : t -> string option

  (** [weight fn] the probability that [fn] is a function *)
  val weight : t -> prob
end
