open Bap_knowledge

type t

val create : ?long:(unit -> string) -> ?desc:string ->
  Knowledge.Name.t -> t

val name : t -> Knowledge.Name.t
val desc : t -> string
val long : t -> string
val pp : Format.formatter -> t -> unit
