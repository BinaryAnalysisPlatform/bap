open Core_kernel
open Regular.Std
open Bap_types.Std

type mem = Bap_memory.t [@@deriving sexp_of]
type kind = Bap_insn_kind.t [@@deriving compare, sexp]

type pred = [
  | `Valid
  |  kind
] [@@deriving sexp]

type reg  [@@deriving bin_io, compare, sexp]
type imm  [@@deriving bin_io, compare, sexp]
type fmm  [@@deriving bin_io, compare, sexp]
type (+'a,+'k) insn
type (+'a,+'k) insns = (mem * ('a,'k) insn option) list
type empty
type asm
type kinds

type full_insn = (asm,kinds) insn [@@deriving compare, sexp_of]
type ('a,'k) t
type (+'a,+'k,'s,'r) state

val with_disasm :
  ?debug_level:int -> ?cpu:string -> backend:string -> string ->
  f:((empty, empty) t -> 'a Or_error.t) -> 'a Or_error.t

val create : ?debug_level:int -> ?cpu:string -> backend:string -> string ->
  (empty, empty) t Or_error.t

val close : (_,_) t -> unit

val store_asm : (_,'k) t -> (asm,'k) t

val store_kinds : ('a,_) t -> ('a,kinds) t
val run :
  ?backlog:int ->
  ?stop_on:pred list ->
  ?invalid:(('a,'k,'s,'r) state -> mem -> 's -> 'r) ->
  ?stopped:(('a,'k,'s,'r) state -> 's -> 'r) ->
  ?hit:(('a,'k,'s,'r) state -> mem -> (asm,kinds) insn -> 's -> 'r) ->
  ('a,'k) t ->
  return:('s -> 'r) ->
  init:'s -> mem -> 'r
val insn_of_mem : (_,_) t -> mem ->
  (mem * (asm,kinds) insn option * [`left of mem | `finished]) Or_error.t

val addr : (_,_,_,_) state -> addr
val preds : (_,_,_,_) state -> pred list
val with_preds : ('a,'k,'s,'r) state -> pred list -> ('a,'k,'s,'r) state
val insns : ('a,'k,_,_) state -> ('a,'k) insns
val last : ('a,'k,'s,'r) state -> int -> ('a,'k) insns
val memory : (_,_,_,_) state -> mem
val stop : (_,_,'s,'r) state -> 's -> 'r
val step : (_,_,'s,'r) state -> 's -> 'r
val jump : (_,_,'s,'r) state -> mem -> 's -> 'r
val back : (_,_,'s,'r) state -> 's -> 'r

module Op : sig
  type t =
    | Reg of reg
    | Imm of imm
    | Fmm of fmm
  [@@deriving bin_io, compare, sexp]

  module Normalized : sig
    val compare : t -> t -> int
    val hash : t -> int
    val compare_ops : t array -> t array -> int
  end

  val pp_adt : Format.formatter -> t -> unit
  include Regular.S with type t := t
end

type op = Op.t [@@deriving bin_io, compare, sexp]

module Insn : sig
  type ('a,'k) t = ('a,'k) insn
  val sexp_of_t : ('a,'k) t -> Sexp.t
  val compare : ('a,'k) t -> ('a,'k) t -> int
  val code : ('a,'k) t -> int
  val name : ('a,'k) t -> string
  val kinds : ('a,kinds) t -> kind list
  val is : ('a,kinds) t -> kind -> bool
  val asm : (asm,'k) t -> string
  val ops  : ('a,'k) t -> op array
end

module Reg : sig
  type t = reg
  val code : t -> int
  val name : t -> string
  include Regular.S with type t := t
end

module Imm : sig
  type t = imm
  val to_word  : t -> width:int -> word option
  val to_int64 : t -> int64
  val to_int   : t -> int option
  include Regular.S with type t := t
end

module Fmm : sig
  type t = fmm
  val to_float : t -> float
  include Regular.S with type t := t
end

module Trie : sig
  type key
  val key_of_first_insns : (_,_,_,_) state -> len:int -> key option

  module Normalized : Trie with type key = key
  include Trie with type key := key
end

val available_backends : unit -> string list
