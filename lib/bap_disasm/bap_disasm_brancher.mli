open Bap_types.Std
open Bap_disasm_source
open Bap_image_std
open Bap_disasm_basic
open Bap_disasm_block

type t
type brancher = t

type dest = addr option * edge [@@deriving sexp]
type dests = dest list [@@deriving sexp]

val create : (mem -> full_insn -> dests) -> t

val of_bil : arch -> t

val of_image : image -> t

val resolve : t -> mem -> full_insn -> dests

val empty : t

val service : Bap_service.service

module Factory : Factory with type t = t
