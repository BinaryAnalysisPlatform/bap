(** Variables  *)
open Core_kernel.Std
open Bap_common

type t

(** implements [Regular] interface  *)
include Regular with type t := t

(** [create ?tmp name typ] creates a fresh new variable with
    assosiated [name] and type [typ]. The created variable is
    absolutely new, and is comparable to true only with itself.

    @param tmp designates variable is temporary, with whatever
    meaning assosiated to it by a caller.
*)

val create : ?tmp:bool -> string -> typ -> t

val renumber : t -> int -> t

val version : t -> int

val base : t -> t

val same : t -> t -> bool

(** [name var] returns a name assosiated with variable  *)
val name : t -> string

(** [typ var] returns a type assosiated with variable  *)
val typ : t -> typ

(** [is_tmp] true if variable is temporary  *)
val is_tmp : t -> bool


(**/**)

module V1 : sig
  type r = string * int * typ * bool
  val serialize   : t -> r
  val deserialize : r -> t
end
