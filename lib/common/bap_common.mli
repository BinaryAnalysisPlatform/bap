

val sites : string -> string list

module Plugins : sig
  val paths : string list
  val list : unit -> string list
  val load_all : unit -> unit
  val load : string -> unit
end
