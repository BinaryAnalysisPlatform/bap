open Bap_strings.Std

open Bap_primus_types
module Machine = Bap_primus_machine

type id [@@deriving bin_io, compare, sexp]
module Id : Base.Comparable.S with type t = id

type t = value [@@deriving bin_io, compare, sexp]

val to_string : t -> string
val of_string : string -> t
val pp : Format.formatter -> t -> unit
val to_word : t -> word

module Index : sig
  val key_width : int
  include Strings.Index.Persistent.S with type key := t
end


type 'a m = Bitvec.modulus -> 'a Machine.t

external (mod) : 'a m -> Bitvec.modulus -> 'a Machine.t = "%apply"

val id : t -> id
val word : word -> t Machine.t
val string : string -> t Machine.t
val bool : bool -> t Machine.t
val one : t Machine.t
val zero : t Machine.t
val int : int -> t m
val int32 : int32 -> t m
val int64 : int64 -> t m
val extract : hi:int -> lo:int -> t -> t Machine.t
val append : int -> int -> t -> t -> t Machine.t
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
val sdiv : t -> t -> t m
val rem : t -> t -> t m
val srem : t -> t -> t m
val smod : t -> t -> t m
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
  val ( % ) : t -> t -> t m
  val ( %$ ) : t -> t -> t m
  val ( %^ ) : t -> t -> t m
  val (lor) : t -> t -> t m
  val (lsl) : t -> t -> t m
  val (lsr) : t -> t -> t m
  val (asr) : t -> t -> t m
  val (lxor) : t -> t -> t m
  val (land) : t -> t -> t m
end

module Symbol : sig
  val to_value : string -> t Machine.t
  val of_value : t -> string Machine.t
end

include Base.Comparable.S with type t := t
