(* Copyright (C) 2017 ForAllSecure, Inc. - All Rights Reserved. *)
(** The type of variables.

    @author Ivan Jager
*)
open Core_kernel

module Type = X86_legacy_bil_type

type t = V of int * string * Type.typ
[@@deriving sexp]
(** The type for a variable identifier.  The int should uniquely
    identify the variable. The string is to make it easier for humans
    to read.  A variable's type is embedded in it.
*)

(** [newvar s t] creates a fresh variable of type [t] and human
    readable string [s]. *)
val newvar : string -> Type.typ -> t


(** [typ v] returns the type of [v]. *)
val typ : t -> Type.typ

(** [name v] returns the name of [v]. *)
val name : t -> string

(** Variable definition and use type. *)
type defuse = {defs : t list;
               uses : t list}

include Comparable.S with type t := t
include Hashable.S with type t := t
