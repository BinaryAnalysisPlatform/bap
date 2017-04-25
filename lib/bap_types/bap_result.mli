open Core_kernel.Std
open Monads.Std
open Regular.Std
open Bap_common

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

type 'a u = (unit,'a) Monad.State.t
type 'a r = (result,'a) Monad.State.t

val undefined : id -> t
val storage : storage -> id -> t
val word : word -> id -> t

val id : t -> id
val value : t -> value

module Value : Printable.S with type t = value

module Storage : sig
  class linear : storage
  class sparse : storage
end

module Id : sig
  include Regular.S with type t = id
  val zero : t
  val succ : t -> t
end

include Printable.S with type t := t
