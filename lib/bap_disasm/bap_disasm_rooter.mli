open Bap_types.Std
open Bap_disasm_source
open Bap_image_std
open Bap_knowledge

type t
type rooter = t

val create : addr seq -> t

val set_path : t -> string -> t

val path : t -> string option

val of_image : image -> t

val of_blocks : (string * addr * addr) seq -> t

val empty : t

val roots : t -> addr seq

val union : t -> t -> t

val provide : t -> unit
val providing : t -> (unit -> 'a knowledge) -> 'a knowledge


module Factory : Factory with type t = t
