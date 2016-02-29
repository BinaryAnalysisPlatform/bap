open Bap_value

module type S = sig
  type t
  val fresh : unit -> t
  val store : t -> unit
end

module Make(T : sig
    type t
    val create : unit -> t
  end) : sig
  include S with type t = T.t
  val state : t ref
end
