open Core_kernel.Std
open Regular.Std
open Bap_future.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std
open Bap_event

type t
type project = t
type pass [@@deriving sexp_of]
type input
type second = float

val create :
  ?disassembler:string ->
  ?cpu:string ->
  ?brancher:brancher source ->
  ?symbolizer:symbolizer source ->
  ?rooter:rooter source ->
  ?reconstructor:reconstructor source ->
  input -> t Or_error.t

val arch : t -> arch
val program : t -> program term
val with_program : t -> program term -> t
val symbols : t -> symtab
val with_symbols : t -> symtab -> t
val storage : t -> dict
val with_storage : t -> dict -> t
val memory : t -> value memmap
val disasm : t -> disasm
val with_memory : t -> value memmap -> t
val tag_memory : t -> mem -> 'a tag -> 'a -> t
val substitute : t -> mem -> string tag  -> string -> t
val set : t -> 'a tag -> 'a -> t
val get : t -> 'a tag -> 'a option
val has : t -> 'a tag -> bool
val del : t -> 'a tag -> t


module Info : sig
  val file : string stream
  val arch : arch stream
  val data : value memmap stream
  val code : value memmap stream
  val cfg : cfg stream
  val symtab : symtab stream
  val program : program term stream
  val spec : Ogre.Doc.t stream
end

module Input : sig
  type t = input
  val file : ?loader:string -> filename:string -> t
  val binary : ?base:addr -> arch -> filename:string -> t

  val create :
    ?finish:(project -> project) ->
    arch ->
    string ->
    code:value memmap ->
    data:value memmap -> t
  val register_loader : string -> (string -> t) -> unit
  val available_loaders : unit -> string list
end

module Pass : sig
  type t = pass [@@deriving sexp_of]

  type error =
    | Unsat_dep of pass * string
    | Runtime_error of pass * exn
    [@@deriving sexp_of]

  exception Failed of error [@@deriving sexp]

  val run : t -> project -> (project,error) Result.t
  val run_exn : t -> project -> project

  val name : t -> string
  val autorun : t -> bool

  val starts    : t -> second stream
  val finishes  : t -> second stream
  val successes : t -> second stream
  val failures  : t -> second stream
end


val find_pass : string -> pass option

val register_pass :
  ?autorun:bool -> ?runonce:bool -> ?deps:string list -> ?name:string
  -> (t -> t) -> unit
val register_pass':
  ?autorun:bool -> ?runonce:bool -> ?deps:string list -> ?name:string
  -> (t -> unit) -> unit


val pass_registrations : pass stream
val passes : unit -> pass list

val restore_state : t -> unit

include Data.S with type t := t
