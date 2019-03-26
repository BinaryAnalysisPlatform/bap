open Core_kernel
open Caml.Format
open Bap_knowledge

module Sort : sig
  type exp =
    | Bool
    | Cons of string * param list
  and param =
    | Sort of exp
    | Index of int
  [@@deriving bin_io, compare, sexp]

  type 'a definition
  type 'a t = 'a definition Knowledge.Class.t

  val define : exp -> 'a -> 'a t
  val exp : 'a t -> exp
  val pp_exp : formatter -> exp -> unit
  val pp : formatter -> 'a t -> unit
end

type 'a sort = 'a Sort.t

module Bool : sig
  type t
  val t : t sort
  val parse : Sort.exp -> t sort option
  val cast : 'a sort -> t sort option
end


module Bitv : sig
  type 'a t
  val define : int -> 'a t sort
  val size : 'a t sort -> int
  val parse : Sort.exp -> 'b t sort option
  val cast : 'a sort -> 'b t sort option
end

module Mem : sig
  type ('a,'b) t
  val define : 'a Bitv.t sort -> 'b Bitv.t sort -> ('a,'b) t sort
  val keys : ('a,'b) t sort -> 'a Bitv.t sort
  val vals : ('a,'b) t sort -> 'b Bitv.t sort
  val parse : Sort.exp -> ('a,'b) t sort option
  val cast : _ sort -> ('a,'b) t sort option
end

module Float : sig
  type 'f t
  type ('r,'s) format

  val define : ('r,'s) format -> ('r,'s) format t sort
  val format : ('r,'s) format t sort -> ('r,'s) format
  val size : ('r,'s) format t sort -> 's Bitv.t sort
  val parse : Sort.exp -> ('r,'s) format t sort option
  val cast : _ sort -> ('r,'s) format t sort option

  module Format : sig
    type ('r,'s) t = ('r,'s) format
    val define : Sort.exp -> 'r -> 's Bitv.t sort -> ('r,'s) format
    val exp : ('r,'s) format -> Sort.exp
    val bits : ('r,'s) format -> 's Bitv.t sort
  end
end

module Rmode : sig
  type t
  val t : t sort
end
