open Core_kernel
open Bap.Std
open Bap_primus_types

module Generator = Bap_primus_generator

val generated : (addr * value) Bap_primus_observation.t

type exn += Pagefault of addr
type memory
module Descriptor : sig
  type t = memory [@@deriving compare, sexp_of]

  val create : addr_size:int -> data_size:int -> string -> memory
  val unknown : addr_size:int -> data_size:int -> memory
  val name : memory -> string
  val addr_size : memory -> int
  val data_size : memory -> int
  include Comparable.S with type t := memory
end

module Make(Machine : Machine) : sig

  val switch : memory -> unit Machine.t
  val memory : memory Machine.t

  val load  : addr -> word Machine.t
  val store : addr -> word -> unit Machine.t

  val get : addr -> value Machine.t
  val set : addr -> value -> unit Machine.t
  val del : addr -> unit Machine.t

  val add_text : mem -> unit Machine.t
  val add_data : mem -> unit Machine.t

  val allocate :
    ?readonly:bool ->
    ?executable:bool ->
    ?init:(addr -> word Machine.t) ->
    ?generator:Generator.t ->
    addr -> int -> unit Machine.t


  val map :
    ?readonly:bool ->
    ?executable:bool ->
    mem -> unit Machine.t


  val is_mapped : addr -> bool Machine.t

  val is_writable : addr -> bool Machine.t
end
