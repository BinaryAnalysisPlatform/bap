open Bap.Std
open Bap_knowledge

type t

type 'a field

val addr : addr field
val name : string field
val ivec : int field


val empty : t
val update : 'a field -> label -> 'a -> t -> t
val lookup : 'a field -> label -> t -> 'a option

val link : 'a field -> 'a -> label Knowledge.t
val resolve : 'a field -> label -> 'a option Knowledge.t

module Syntax : sig
  val link_addr : addr -> label Knowledge.t
  val link_name : string -> label Knowledge.t
  val link_ivec : int -> label Knowledge.t

  val resolve_addr : label -> addr option Knowledge.t
  val resolve_name : label -> string option Knowledge.t
  val resolve_ivec : label -> int option Knowledge.t
end
