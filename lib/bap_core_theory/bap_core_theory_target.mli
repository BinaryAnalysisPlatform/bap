open Core_kernel
open Bap_knowledge

module KB = Knowledge
module Var = Bap_core_theory_var
module Mem = Bap_core_theory_value.Mem

type t [@@deriving bin_io, compare, sexp]
type endianness
type system
type abi
type fabi
type filetype
type role
type options = (options_cls,unit) KB.Class.t KB.Value.t and options_cls

val declare :
  ?parent:t ->
  ?bits:int ->
  ?byte:int ->
  ?data:_ Mem.t Var.t ->
  ?code:_ Mem.t Var.t ->
  ?vars:unit Var.t list ->
  ?regs:(role list * unit Var.t list) list ->
  ?endianness:endianness ->
  ?system:system ->
  ?abi:abi ->
  ?fabi:fabi ->
  ?filetype:filetype ->
  ?options:options ->
  ?nicknames:string list ->
  ?package:string ->
  string -> t

val get : ?package:string -> string -> t
val read : ?package:string -> string -> t
val lookup : ?package:string -> string -> t option
val unknown : t
val is_unknown : t -> bool
val name : t -> KB.Name.t
val matches : t -> string -> bool
val order : t -> t -> KB.Order.partial
val belongs : t -> t -> bool
val parent : t -> t
val parents : t -> t list
val declared : unit -> t list
val family : t -> t list
val partition : t list -> t list list
val families : unit -> t list list
val bits : t -> int
val byte : t -> int
val data_addr_size : t -> int
val code_addr_size : t -> int
val data : t -> (unit,unit) Mem.t Var.t
val code : t -> (unit,unit) Mem.t Var.t
val vars : t -> Set.M(Var.Top).t
val regs :
  ?exclude:role list ->
  ?roles:role list ->
  t -> Set.M(Var.Top).t
val reg : ?exclude:role list -> ?unique:bool -> t -> role -> unit Var.t option
val endianness : t -> endianness
val system : t -> system
val abi : t -> abi
val fabi : t -> fabi
val filetype : t -> filetype
val options : t -> options

val domain : t KB.Domain.t
val persistent : t KB.Persistent.t

module Endianness : sig
  include KB.Enum.S with type t = endianness
  val le : endianness
  val eb : endianness
  val bi : endianness
end

module Role : sig
  type t = role
  module Register : sig
    val general : t
    val special : t
    val pseudo : t
    val integer : t
    val floating : t
    val vector : t
    val stack_pointer : t
    val frame_pointer : t
    val link : t
    val thread : t
    val privileged : t
    val constant : t
    val zero : t
    val status : t
    val zero_flag : t
    val sign_flag : t
    val carry_flag : t
    val overflow_flag : t
    val parity_flag : t
    val hardware : t
    val reserved : t
    val function_argument : t
    val function_return : t
    val caller_saved : t
    val callee_saved : t
  end

  include KB.Enum.S with type t := t
end

module System : KB.Enum.S with type t = system
module Filetype : KB.Enum.S with type t = filetype
module Abi : KB.Enum.S with type t = abi
module Fabi : KB.Enum.S with type t = fabi

module Options : sig
  type cls = options_cls
  include KB.Value.S with type t = options
  include Binable.S with type t := t
  include Pretty_printer.S with type t := t
  val cls : (cls, unit) KB.cls
  val to_string : t -> string
end

include Base.Comparable.S with type t := t
include Binable.S with type t := t
include Stringable.S with type t := t
include Pretty_printer.S with type t := t
val name : t -> KB.Name.t
val unknown : t
val domain : t KB.domain
val persistent : t KB.persistent
