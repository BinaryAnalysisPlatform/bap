open Core_kernel.Std

module Std : sig
  type demangler
  module Demangler : sig
    type t = demangler
    val create : string -> (string -> string) -> t
    val run : t -> string -> string
    val name : t -> string
  end

  module Demanglers : sig
    val register : demangler -> unit
    val available : unit -> demangler list
  end
end
