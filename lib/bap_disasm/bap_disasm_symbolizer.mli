open Bap_types.Std
open Bap_image_std
open Bap_disasm_source

type t
type symbolizer = t

val empty : t

val create : (addr -> string option) -> t

val of_blocks : (string * addr * addr) seq -> t

val of_image : image -> t

val resolve : t -> addr -> string

val chain : t list -> t

module Factory : Factory with type t = t
