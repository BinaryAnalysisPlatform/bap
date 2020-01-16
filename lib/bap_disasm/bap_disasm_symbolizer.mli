open Bap_knowledge
open Bap_core_theory
open Bap_types.Std
open Bap_image_std
open Bap_disasm_source

type t
type symbolizer = t

val provide : Knowledge.agent -> t -> unit
val get_name : addr -> string knowledge

module Toplevel : sig
  val get_name : addr -> string
end

val empty : t

val create : (addr -> string option) -> t

val of_blocks : (string * addr * addr) seq -> t

val of_image : image -> t

val resolve : t -> addr -> string

val chain : t list -> t

module Name : sig
  val is_empty : string -> bool
  val order : string -> string -> Knowledge.Order.partial
end

module Factory : Factory with type t = t
