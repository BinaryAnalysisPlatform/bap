open Core_kernel.Std
open Bap.Std
open Microx.Std

exception Entry_point_not_found

class type result = object
  method visited : int Tid.Map.t
  method tainted_regs : tid -> Taint.map
  method tainted_ptrs : tid -> Taint.map
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
