open Core_kernel
open Bap.Std


(** execution context.

    The context defines the state of execution.

    @param max_steps the maximum number of steps, that should be taken
    in a single path.

    @param max_loop the maximum number of loop iterations (note only
    loops inner to a function are detected, so recursive loops may
    stall the interpreter.
*)
class context :
  ?max_steps:int ->
  ?max_loop:int ->
  program term -> object('s)
    [@@@warning "-D"]
    inherit Biri.context

    method visited : int Tid.Map.t

    method add_checkpoint : tid -> 's
    method checkpoints : 's Tid.Map.t Tid.Map.t
    method backtrack : 's option
    method merge : 's -> 's

    method return : 's
    method store_return : tid -> 's

    method visit_term : tid -> 's
    method enter_sub : sub term -> 's
    method leave_sub : sub term -> 's
    method enter_blk : blk term -> 's
    method blk : blk term option
    method step : 's option
    method will_loop : tid -> bool
    method will_return : tid -> bool
  end


(** BIR interpreter, that executes a program in a given context.

    @param deterministic if is [true] then only feasible execution
    path is taken, otherwise, the interpreter, will execute all
    linearly independent paths.

  *)
class ['a] main : ?deterministic:bool -> program term -> object
    inherit ['a] biri
    constraint 'a = #context
  end
