open Core_kernel
open Bap.Std

type t = [
  | `xF0 (** LOCK                *)
  | `xF2 (** REPNE               *)
  | `xF3 (** REP/REPE            *)
  | `x2E (** CS segment override *)
  | `x36 (** SS segment override *)
  | `x3E (** DS segment override *)
  | `x26 (** ES segment override *)
  | `x64 (** FS segment override *)
  | `x65 (** GS segment override *)
  | `x66 (** operand override    *)
  | `x67 (** address override    *)
] [@@deriving bin_io, sexp, compare, enumerate]

(** [get mem] - returns a list of prefixes, if any *)
val get : mem -> t list

(** [exists prefix mem] - returns true if prefix exists in mem *)
val exists : t -> mem -> bool

(** [fold ~init ~f mem] - deref memory and fold over prefixes *)
val fold: init:'a -> f:('a -> t -> 'a) -> mem -> 'a
