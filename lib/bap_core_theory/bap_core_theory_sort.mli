open Core_kernel
open Caml.Format

type bit
type 'a bitv
type ('a,'b) mem
type 'a sort
type ('e,'s) float
type rmode

module Sort : sig
  type 'a t = 'a sort

  type exp =
    | Bool
    | Cons of string * param list
  and param =
    | Sort of exp
    | Index of int
  [@@deriving compare, sexp]

  val define : exp -> 'a -> 'a t
  val name : 'a t -> string
  val exp : 'a t -> exp
  val type_equal : 'a t -> 'b t -> ('a t, 'b t) Type_equal.t option
  val same : 'a t -> 'b t -> bool
  val pp_exp : formatter -> exp -> unit
  val pp : formatter -> 'a t -> unit
  val compare : 'a t -> 'a t -> int
end

module Bool : sig
  type t = bit
  val t : bit sort
  val parse : Sort.exp -> bit sort option
  val cast : 'a sort -> bit sort option
end


module Bits : sig
  type 'a t = 'a bitv
  val define : int -> 'a bitv sort
  val size : 'a bitv sort -> int
  val parse : Sort.exp -> 'b bitv sort option
  val cast : 'a sort -> 'b bitv sort option
end

module Mems : sig
  type ('a,'b) t = ('a,'b) mem
  val define : 'a bitv sort -> 'b bitv sort -> ('a,'b) mem sort
  val keys : ('a,'b) mem sort -> 'a bitv sort
  val vals : ('a,'b) mem sort -> 'b bitv sort
  val parse : Sort.exp -> ('a,'b) mem sort option
  val cast : _ sort -> ('a,'b) mem sort option
end

module Floats : sig
  type ('e,'s) t = ('e,'s) float
  val define : 'e bitv sort -> 's bitv sort -> ('e,'s) t sort
  val exps : ('e,'s) t sort -> 'e bitv sort
  val sigs : ('e,'s) t sort -> 's bitv sort
end

module Rmode : sig
  type t = rmode
  val t : t sort
end
