open Core_kernel.Std
open Bap.Std

class context :
  ?max_steps:int ->
  ?max_loop:int ->
  program term -> object('s)
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

class ['a] main : ?deterministic:bool -> program term -> object
    inherit ['a] biri
    constraint 'a = #context
  end
