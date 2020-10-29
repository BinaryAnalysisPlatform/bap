open Bap_knowledge
open Bap_core_theory
open Core_kernel
open Regular.Std
open Bap_future.Std
open Bap_types.Std
open Bap_image_std
open Bap_disasm_std
open Bap_sema.Std

type t
type project = t
type pass [@@deriving sexp_of]
type input
type state [@@deriving bin_io]
type second = float

val state : t -> state

val empty : Theory.Target.t -> t

val create :
  ?package:string ->
  ?state:state ->
  ?disassembler:string ->
  ?brancher:brancher source ->
  ?symbolizer:symbolizer source ->
  ?rooter:rooter source ->
  ?reconstructor:reconstructor source ->
  input -> t Or_error.t

val arch : t -> arch
val target : t -> Theory.Target.t
val specification : t -> Ogre.doc
val program : t -> program term
val with_program : t -> program term -> t
val symbols : t -> symtab
val with_symbols : t -> symtab -> t
val storage : t -> dict
val with_storage : t -> dict -> t
val memory : t -> value memmap
val memory_slot : (Theory.Unit.cls, value Memmap.t) KB.slot
val disasm : t -> disasm
val with_memory : t -> value memmap -> t
val tag_memory : t -> mem -> 'a tag -> 'a -> t
val substitute : t -> mem -> string tag  -> string -> t
val set : t -> 'a tag -> 'a -> t
val get : t -> 'a tag -> 'a option
val has : t -> 'a tag -> bool
val del : t -> 'a tag -> t



val map_program : t -> f:(program term -> program term) -> t

module State : sig
  type t = state
  val disassembly : t -> Bap_disasm_driver.state
  val subroutines : t -> Bap_disasm_calls.t
  val slot : (Theory.Unit.cls, state) KB.slot
end

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

  val load : ?target:Theory.Target.t -> ?loader:string -> string -> t
  val custom :
    ?finish:(project -> project) ->
    ?filename:string ->
    ?code:value memmap ->
    ?data:value memmap ->
    Theory.Target.t -> t

  val raw_file : ?base:addr -> Theory.Target.t -> string -> t
  val from_string : ?base:addr -> Theory.Target.t -> string -> t
  val from_bigstring : ?base:addr -> Theory.Target.t -> Bigstring.t -> t

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

module Collator : sig
  type t
  type info

  val apply : t -> project seq -> unit
  val find : ?package:string -> string -> t option
  val name : info -> Knowledge.Name.t
  val desc : info -> string

  val register : ?desc:string -> ?package:string -> string ->
    prepare:(project -> 's) ->
    collate:(int -> 's -> project -> 's) ->
    summary:('s -> unit) ->
    unit

  val registered : unit -> info list
end

module Analysis : sig
  type t
  type info
  type grammar
  type 'a arg
  type ('a,'r) args

  val args : 'a arg -> ('a -> 'b, 'b) args
  val ($) : ('a, 'b -> 'c) args -> 'b arg -> ('a,'c) args

  val empty : unit arg
  val string : string arg
  val bitvec : Bitvec.t arg
  val program : Theory.Label.t arg
  val unit : Theory.Unit.t arg

  val optional : 'a arg -> 'a option arg
  val keyword : string -> 'a arg -> 'a option arg
  val flag : string -> bool arg
  val rest : 'a arg -> 'a list arg

  val register : ?desc:string -> ?package:string -> string ->
    ('a,unit knowledge) args -> 'a -> unit

  val registered : unit -> info list

  val apply : t -> string list -> unit knowledge
  val find : ?package:string -> string -> t option
  val name : info -> Knowledge.Name.t
  val desc : info -> string
  val grammar : info -> grammar

  val argument :
    ?desc:string ->
    parse:(fail:(string -> _ knowledge) -> string -> 'a knowledge) ->
    string -> 'a arg

  module Grammar : sig
    type t = grammar
    val to_string : grammar -> string
  end

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
