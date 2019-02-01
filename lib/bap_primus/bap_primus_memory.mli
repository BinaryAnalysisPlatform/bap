open Base
open Bigarray
open Bap_primus_types

module Generator = Bap_primus_generator
module Machine = Bap_primus_machine

type exn += Pagefault of addr
type memory

type data = (char, int8_unsigned_elt, c_layout) Array1.t


module Descriptor : sig
  type t = memory [@@deriving compare, sexp_of]

  val create : addr_size:int -> data_size:int -> string -> memory
  val unknown : addr_size:int -> data_size:int -> memory
  val name : memory -> string

  include Comparable.S with type t := memory
end

val switch : memory -> unit Machine.t
val memory : memory Machine.t

val load  : addr -> word Machine.t
val store : addr -> word -> unit Machine.t

val get : addr -> value Machine.t
val set : addr -> value -> unit Machine.t

val add_text : addr -> data -> unit Machine.t
val add_data : addr -> data -> unit Machine.t

val allocate :
  ?executable:bool ->
  ?readonly:bool ->
  ?init:(addr -> word Machine.t) ->
  ?generator:Generator.t ->
  addr -> int -> unit Machine.t


val map :
  ?executable:bool ->
  ?readonly:bool ->
  ?reversed:bool ->
  addr -> data -> unit Machine.t

val is_mapped : addr -> bool Machine.t

val is_writable : addr -> bool Machine.t
