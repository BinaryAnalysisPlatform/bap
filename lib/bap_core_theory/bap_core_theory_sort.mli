open Core_kernel
open Caml.Format
open Bap_knowledge

module KB = Knowledge

module Sort : sig
  type +'a exp
  type +'a sym
  type +'a num
  type name

  type 'a t = 'a exp KB.cls
  type top = unit t

  val sym : name -> 'a sym exp
  val int : int -> 'a num exp
  val app : 'a exp -> 'b exp -> ('a -> 'b) exp
  val (@->) : 'a exp -> 'b exp -> ('a -> 'b) exp

  val t : 'a exp KB.Class.abstract KB.Class.t

  val exp : 'a t -> 'a exp

  val value : 'a num exp -> int
  val name :  'a sym exp -> name

  val hd : ('a -> 'b) exp -> 'a exp
  val tl : ('a -> 'b) exp -> 'b exp

  val pp : formatter -> 'a t -> unit

  val forget : 'a t -> unit t

  module Top : sig
    type t = top [@@deriving bin_io, compare, sexp]
    include Base.Comparable.S with type t := t
  end

  module Name : sig
    type t
    val declare : ?package:string -> string -> name
    include Base.Comparable.S with type t := t
  end
end

type 'a sort = 'a Sort.t

module Bool : sig
  type t
  val t : t sort
  val refine : unit sort -> t sort option
end


module Bitv : sig
  type 'a t
  val define : int -> 'a t sort
  val refine : unit sort -> 'a t sort option
  val size : 'a t sort -> int
end

module Mem : sig
  type ('a,'b) t
  val define : 'a Bitv.t sort -> 'b Bitv.t sort -> ('a,'b) t sort
  val refine : unit sort -> ('a,'b) t sort option
  val keys : ('a,'b) t sort -> 'a Bitv.t sort
  val vals : ('a,'b) t sort -> 'b Bitv.t sort
end

module Float : sig
  module Format : sig
    type ('r,'s) t
    val define : 'r Sort.exp -> 's Bitv.t sort -> ('r,'s) t Sort.exp
    val bits : ('r,'s) t Sort.exp -> 's Bitv.t sort
    val exp : ('r,'s) t Sort.exp -> 'r Sort.exp
  end

  type ('r,'s) format = ('r,'s) Format.t
  type 'f t

  val define : ('r,'s) format Sort.exp -> ('r,'s) format t sort
  val refine : unit sort -> ('r,'s) format t sort option
  val format : ('r,'s) format t sort -> ('r,'s) format Sort.exp
  val size : ('r,'s) format t sort -> 's Bitv.t sort
end

module Rmode : sig
  type t
  val t : t sort
  val refine : unit sort -> t sort option
end