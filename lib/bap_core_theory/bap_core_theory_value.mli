open Core_kernel
open Caml.Format
open Bap_knowledge

module KB = Knowledge

type +'a sort
type cls

type 'a t = (cls,'a sort) KB.cls KB.value
val cls : (cls,unit) KB.cls

val empty : 'a sort -> 'a t
val sort : 'a t -> 'a sort

module Sort : sig
  type +'a t = 'a sort
  type +'a sym
  type +'a num
  type name
  type cls

  val sym : name -> 'a sym sort
  val int : int -> 'a num sort
  val app : 'a sort -> 'b sort -> ('a -> 'b) sort
  val (@->) : 'a sort -> 'b sort -> ('a -> 'b) sort

  val value : 'a num sort -> int
  val name :  'a sym sort -> name

  val hd : ('a -> 'b) sort -> 'a sort
  val tl : ('a -> 'b) sort -> 'b sort


  val forget : 'a t -> unit t
  val refine : name -> unit sort -> 'a t option
  val same : 'a t -> 'b t -> bool

  val pp : formatter -> 'a t -> unit

  module Top : sig
    type t = unit sort [@@deriving bin_io, compare, sexp]
    include Base.Comparable.S with type t := t
  end

  module Name : sig
    type t
    val declare : ?package:string -> string -> name
    include Base.Comparable.S with type t := t
  end
end

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
    val define : 'r Sort.t -> 's Bitv.t sort -> ('r,'s) t Sort.t
    val bits : ('r,'s) t Sort.t -> 's Bitv.t sort
    val exp : ('r,'s) t Sort.t -> 'r Sort.t
  end

  type ('r,'s) format = ('r,'s) Format.t
  type 'f t

  val define : ('r,'s) format Sort.t -> ('r,'s) format t sort
  val refine : unit sort -> ('r,'s) format t sort option
  val format : ('r,'s) format t sort -> ('r,'s) format Sort.t
  val bits : ('r,'s) format t sort -> 's Bitv.t sort
end

module Rmode : sig
  type t
  val t : t sort
  val refine : unit sort -> t sort option
end
