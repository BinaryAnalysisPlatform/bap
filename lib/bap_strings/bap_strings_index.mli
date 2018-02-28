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

    (** [empty] an empty index *)
    val empty : t

    (** [string idx key] returns data associated with the
        provided [key]. Returns an empty string if there are no data.
    *)
    val string : t -> key -> string

    (** [key idx data] returns the key associated with [data], if no
        key is associated then [Key.null] is returned *)
    val key : t -> string -> key

    (** [register idx data] registers data in the index [idx]. If
        data is already registered, then the index is returned
        unchanged. Otherwise returns an index where a fresh new key is
        associated with [data].  *)
    val register : t -> string -> t

    (** [registered idx data] is [true] if [data] was registered in
        the index. *)
    val registered : t -> string -> bool
  end

  (** Make(Key) creates an index with the given [Key]. *)
  module Make (Key : Key) : S with type key := Key.t
end
