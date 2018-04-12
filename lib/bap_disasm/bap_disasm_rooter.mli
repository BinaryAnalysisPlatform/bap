open Bap_types.Std
open Bap_disasm_source
open Bap_image_std

type t
type rooter = t

val create : addr seq -> t

val of_image : image -> t

val of_blocks : (string * addr * addr) seq -> t

val empty : t

val roots : t -> addr seq

val union : t -> t -> t

val service : Bap_service.service

module Factory : Factory with type t = t
