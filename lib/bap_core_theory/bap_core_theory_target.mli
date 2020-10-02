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
type options = (options_cls,unit) KB.Class.t KB.Value.t and options_cls

val declare :
  ?parent:t ->
  ?bits:int ->
  ?byte:int ->
  ?data:_ Mem.t Var.t ->
  ?code:_ Mem.t Var.t ->
  ?vars:unit Var.t list ->
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
val endianness : t -> endianness
val system : t -> system
val abi : t -> abi
val fabi : t -> fabi
val filetype : t -> filetype
val options : t -> options

val domain : t KB.Domain.t
val persistent : t KB.Persistent.t

module Enum : sig
  module type S = sig
    include Base.Comparable.S
    include Binable.S with type t := t
    include Stringable.S with type t := t
    include Pretty_printer.S with type t := t
    include Sexpable.S with type t := t
    val declare : ?package:string -> string -> t
    val read : ?package:string -> string -> t
    val name : t -> KB.Name.t
    val unknown : t
    val is_unknown : t -> bool
    val domain : t KB.domain
    val persistent : t KB.persistent
    val hash : t -> int
    val members : unit -> t list
  end

  module Make() : S
end

module Endianness : sig
  include Enum.S with type t = endianness
  val le : endianness
  val eb : endianness
  val bi : endianness
end

module System : Enum.S with type t = system
module Filetype : Enum.S with type t = filetype
module Abi : Enum.S with type t = abi
module Fabi : Enum.S with type t = fabi

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
