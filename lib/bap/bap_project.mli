open Core_kernel.Std
open Regular.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std

type t

val from_file :
  ?on_warning:(Error.t -> unit Or_error.t) ->
  ?loader:string ->
  ?disassembler:string ->
  ?brancher:brancher ->
  ?symbolizer:symbolizer ->
  ?rooter:rooter ->
  ?reconstructor:reconstructor ->
  string -> t Or_error.t

val from_image :
  ?disassembler:string ->
  ?brancher:brancher ->
  ?symbolizer:symbolizer ->
  ?rooter:rooter ->
  ?reconstructor:reconstructor ->
  image -> t Or_error.t

val from_mem :
  ?disassembler:string ->
  ?brancher:brancher ->
  ?symbolizer:symbolizer ->
  ?rooter:rooter ->
  ?reconstructor:reconstructor ->
  arch -> mem -> t Or_error.t

val from_string :
  ?base:addr ->
  ?disassembler:string ->
  ?brancher:brancher ->
  ?symbolizer:symbolizer ->
  ?rooter:rooter ->
  ?reconstructor:reconstructor ->
  arch -> string -> t Or_error.t

val from_bigstring :
  ?base:addr ->
  ?disassembler:string ->
  ?brancher:brancher ->
  ?symbolizer:symbolizer ->
  ?rooter:rooter ->
  ?reconstructor:reconstructor ->
  arch -> Bigstring.t -> t Or_error.t

val arch : t -> arch
val program : t -> program term
val with_program : t -> program term -> t
val symbols : t -> symtab
val with_symbols : t -> symtab -> t
val memory : t -> value memmap
val disasm : t -> disasm
val with_memory : t -> value memmap -> t
val tag_memory : t -> mem -> 'a tag -> 'a -> t
val substitute : t -> mem -> string tag  -> string -> t
val set : t -> 'a tag -> 'a -> t
val get : t -> 'a tag -> 'a option
val has : t -> 'a tag -> bool

val restore_state : t -> unit

type error =
  | Not_loaded of string
  | Runtime_error of string * exn
with sexp_of

exception Pass_failed of error with sexp

val register_pass : ?deps:string list -> ?name:string -> (t -> t) -> unit
val register_pass': ?deps:string list -> ?name:string -> (t -> unit) -> unit

val passes : unit -> string list Or_error.t
val run_pass : t -> string -> t Or_error.t
val run_pass_exn : t -> string -> t
val passes : unit -> string list

module Factory : Source.Factory
  with type t =
         ?disassembler:string ->
         ?brancher:brancher ->
         ?symbolizer:symbolizer ->
         ?rooter:rooter ->
         ?reconstructor:reconstructor -> unit -> t Or_error.t
include Data with type t := t
