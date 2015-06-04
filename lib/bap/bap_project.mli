open Core_kernel.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std

type t

val from_file :
  ?on_warning:(Error.t -> unit Or_error.t) ->
  ?backend:string ->
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  string -> t Or_error.t

val from_image :
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  image -> t Or_error.t

val from_mem :
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  arch -> mem -> t Or_error.t

val from_string :
  ?base:addr ->
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  arch -> string -> t Or_error.t

val from_bigstring :
  ?base:addr ->
  ?name:(addr -> string option) ->
  ?roots:addr list ->
  arch -> Bigstring.t -> t Or_error.t

val arch : t -> arch
val program : t -> program term
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

type 'a register = ?deps:string list -> string -> 'a -> unit
type error =
  | Not_loaded of string
  | Is_duplicate of string
  | Not_found of string
  | Doesn't_register of string
  | Load_failed of string * Error.t
  | Runtime_error of string * exn
with sexp_of

exception Pass_failed of error with sexp

val register_pass : (t -> t) register
val register_pass': (t -> unit) register
val register_pass_with_args : (string array -> t -> t) register
val register_pass_with_args' : (string array -> t -> unit) register

val passes : ?library:string list -> unit -> string list Or_error.t
val run_passes : ?library:string list -> ?argv:string array -> t -> t Or_error.t

val passes_exn : ?library:string list -> unit -> string list
val run_passes_exn : ?library:string list -> ?argv:string array -> t -> t
