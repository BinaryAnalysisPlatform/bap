open Core_kernel.Std
open Regular.Std
open Bap.Std

open Bap_primus_types

type id [@@deriving bin_io, compare, sexp]
module Id : Regular.S with type t = id

type t = value [@@deriving bin_io, compare, sexp]

val to_word : t -> word
val id : t -> id


module Make(Machine : Machine) : sig
  type t = value
  type 'a m = 'a Machine.t
  val id : t -> id
  val to_word : t -> word
  val of_word : word -> t m
  val of_string : string -> t m
  val of_bool : bool -> t m
  val of_int : width:int -> int -> t m
  val of_int32 : ?width:int -> int32 -> t m
  val of_int64 : ?width:int -> int64 -> t m
  val b0 : t m
  val b1 : t m
  val one : int -> t m
  val zero : int -> t m
  val signed : t -> t m
  val is_zero : t -> bool
  val is_one : t -> bool
  val is_positive : t -> bool
  val is_negative : t -> bool
  val is_non_positive : t -> bool
  val is_non_negative : t -> bool
  val bitwidth : t -> int
  val extract : ?hi:int -> ?lo:int -> t -> t m
  val concat : t -> t -> t m
  val succ : t -> t m
  val pred : t -> t m
  val nsucc : t -> int -> t m
  val npred : t -> int -> t m
  val abs : t -> t m
  val neg : t -> t m
  val add : t -> t -> t m
  val sub : t -> t -> t m
  val mul : t -> t -> t m
  val div : t -> t -> t m
  val modulo : t -> t -> t m
  val lnot : t -> t m
  val logand : t -> t -> t m
  val logor : t -> t -> t m
  val logxor : t -> t -> t m
  val lshift : t -> t -> t m
  val rshift : t -> t -> t m
  val arshift : t -> t -> t m

  module Syntax : sig
    val ( ~-) : t -> t m
    val ( + ) : t -> t -> t m
    val ( - ) : t -> t -> t m
    val ( * ) : t -> t -> t m
    val ( / ) : t -> t -> t m
    val (mod) : t -> t -> t m
    val (lor) : t -> t -> t m
    val (lsl) : t -> t -> t m
    val (lsr) : t -> t -> t m
    val (asr) : t -> t -> t m
    val (lxor) : t -> t -> t m
    val (land) : t -> t -> t m
  end
  include Regular.S with type t := t
end

include Regular.S with type t := t
