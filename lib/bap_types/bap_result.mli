open Core_kernel.Std
open Bap_common
open Bap_monad

class type storage = object('s)
  method load : addr -> word option
  method save : addr -> word -> 's
end

type value =
  | Imm of word
  | Mem of storage
  | Bot


type result
type id

type t = result

type 'a u = (unit,'a) State.t
type 'a r = (result,'a) State.t

val undefined : id -> t
val storage : storage -> id -> t
val word : word -> id -> t

val id : t -> id
val value : t -> value

module Value : Printable with type t = value

module Storage : sig
  class linear : storage
  class sparse : storage
end

module Id : sig
  include Regular with type t = id
  val zero : t
  val succ : t -> t
end

include Printable with type t := t
