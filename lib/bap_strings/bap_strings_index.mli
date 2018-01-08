module type Key = sig 
  type t
  val compare : t -> t -> int
  val null : t 
  val succ : t -> t
end

module Persistent : sig 
  module type S = sig
    type t 
    type key
    val string : t -> key -> string
    val key : t -> string -> key
    val register : t -> string -> t
    val registered : t -> string -> bool
  end
  module Make (Key : Key) : S with type key := Key.t
end
