open Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

module Elementary : sig

  type 'a t = 'a knowledge

  exception Not_a_table

  val approximate :
    rank : int ->
    reduce : (('a,'s) format float value t -> 'r bitv value t) ->
    extract : (int -> 'd bitv value t -> 's bitv value t) ->
    coefs : ('r, 'd) mem var ->
    rmode : rmode value t ->
    ('a, 's) format float value t ->
    ('a, 's) format float value t

  val tabulate :
    string ->
    rank:int ->
    interaval:int ->
    ('a, 's) format float value t ->
    ('a, 's) format float value t

  val is_table : ('r, 'd) mem var -> bool

  val operation : ('r, 'd) mem var -> string

  val table : string -> 'a sort -> int -> string

  module Scheme : sig
    type 'a t = 'a sort -> int -> string

    val pow : t
    val powr : t
    val compound : t
    val rootn : t
    val pownn : t
    val rsqrt : t
    val hypot : t
    val exp  : t
    val expm1 : t
    val exp2 : t
    val exp2m1 : t
    val exp10 : t
    val exp10m1 :t
    val log : t
    val log2 : t
    val log10 : t
    val logp1 : t
    val log2p1 : t
    val log10p1 : t
    val sin : t
    val cos : t
    val tan : t
    val sinpi : t
    val cospi : t
    val atanpi : t
    val atan2pi : t
    val asin : t
    val acos : t
    val atan : t
    val atan2 : t
    val sinh : t
    val cosh : t
    val tanh : t
    val asinh : t
    val acosh : t
    val atanh : t

  end
end
