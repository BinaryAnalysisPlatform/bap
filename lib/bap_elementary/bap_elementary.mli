open Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

(** Elementary is a library that provides few primitives for
    approximations of floating point operations via table methods.  *)
module Elementary (Theory : Theory.Core) : sig

  type 'a t = 'a knowledge

  exception Not_a_table

  (** [approximate ~rank ~reduce ~extract ~coefs x rmode]
      returns a function f(x,rmode) that is defined by a polynomial
      of rank [rank], which coefficients are stored in a table [coefs].

      @param coefs is a table, where keys are some points in integer
      space, in which floating point values could be mapped by
      [reduce] function. And values of [coefs] are some point in
      integer namespace from which floating point coefficients
      could be restored by [extract] function with a respect to a
      rank of each coefficient. *)
  val approximate :
    rank : int ->
    reduce : (('a,'s) format float value t -> 'r bitv value t) ->
    extract : (int -> 'd bitv value t -> 's bitv value t) ->
    coefs : ('r, 'd) mem var ->
    ('a, 's) format float value t ->
    rmode value t ->
    ('a, 's) format float value t

  (** [tabulate op ~rank ~size x rmode] defines a subset of
      functions that can be created by [approximate], s.t.
      each value in a table is a concatenation of coefficients
      (from one with the least rank to one with the most),
      and each key is first [size] bits of floating point value.

      @param op is a name of floating point operation to approximate. *)
  val tabulate :
    string ->
    rank:int ->
    size:int ->
    ('a, 's) format float value t ->
    rmode value t ->
    ('a, 's) format float value t

  (** [table operation sort rank] defines a naming scheme for
      approximation of [rank] of an [operation] for values of [sort].  *)
  val table : string -> ('r, 's) format float sort -> int -> string

  (** [is_table ident] returns true if [ident] is a table *)
  val is_table  : Var.ident -> bool

  (** [operation ident] returns the name of a function,
      which polynomial coefficients reside in a table
      referenced by [ident].
      Raise Not_a_table if [ident] doesn't match to a naming
      scheme *)
  val operation : Var.ident -> string

  (** module contains naming schemes for different math functions *)
  module Scheme : sig
    type 'a t = 'a sort -> int -> string

    val pow : 'a t
    val powr : 'a t
    val compound : 'a t
    val rootn : 'a t
    val pownn : 'a t
    val rsqrt : 'a t
    val hypot : 'a t
    val exp  : 'a t
    val expm1 : 'a t
    val exp2 : 'a t
    val exp2m1 : 'a t
    val exp10 : 'a t
    val exp10m1 : 'a t
    val log : 'a t
    val log2 : 'a t
    val log10 : 'a t
    val logp1 : 'a t
    val log2p1 : 'a t
    val log10p1 : 'a t
    val sin : 'a t
    val cos : 'a t
    val tan : 'a t
    val sinpi : 'a t
    val cospi : 'a t
    val atanpi : 'a t
    val atan2pi : 'a t
    val asin : 'a t
    val acos : 'a t
    val atan : 'a t
    val atan2 : 'a t
    val sinh : 'a t
    val cosh : 'a t
    val tanh : 'a t
    val asinh : 'a t
    val acosh : 'a t
    val atanh : 'a t
  end

end
