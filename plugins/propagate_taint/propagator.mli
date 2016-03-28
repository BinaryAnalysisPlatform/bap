open Core_kernel.Std
open Regular.Std
open Bap.Std
open Microx.Std

exception Entry_point_not_found

type result
  [@@deriving bin_io, compare, sexp]

module Result : sig
  type t = result [@@deriving bin_io, compare, sexp]
  val empty : t
  val union : t -> t -> t
  val tainted_regs : t -> tid -> Taint.map
  val tainted_ptrs : t -> tid -> Taint.map
  val is_tainted : t -> tid -> bool
  val is_visited : t -> tid -> bool
  val visited : t -> int Tid.Map.t
  include Regular with type t := t
end


val run :
  max_steps : int ->
  max_loop : int ->
  deterministic : bool ->
  ?random_seed:int ->
  reg_policy : Concretizer.policy ->
  mem_policy : Concretizer.policy ->
  project -> [
    | `Name of string
    | `Term of tid] -> result
