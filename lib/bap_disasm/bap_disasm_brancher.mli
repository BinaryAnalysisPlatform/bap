open Bap_types.Std
open Bap_disasm_source
open Bap_image_std
open Bap_disasm_basic
open Bap_disasm_block
open Bap_knowledge

type t
type brancher = t

type dest = addr option * edge [@@deriving sexp]
type dests = dest list [@@deriving sexp]

val create : (mem -> full_insn -> dests) -> t

val set_path : t -> string -> t

val path : t -> string option

val of_bil : arch -> t

val of_image : image -> t

val resolve : t -> mem -> full_insn -> dests

val empty : t

val provide : t -> unit

val providing : t -> (unit -> 'a knowledge) -> 'a knowledge

module Factory : Factory with type t = t
