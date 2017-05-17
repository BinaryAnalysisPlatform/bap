open Core_kernel.Std
open Monads.Std
open Bap_common
open Bap_result
open Bap_bil
open Bap_type_error



module Eval = struct

  module Class(State : T2) = struct
    type ('a,'e) state = ('a,'e) State.t

    class type ['a,'r] semantics = object
      method eval_exp : exp -> ('r,'a) state
      method eval_var : var -> ('r,'a) state
      method eval_int : word -> ('r,'a) state
      method eval_load : mem:exp -> addr:exp -> endian -> size -> ('r,'a) state
      method eval_store : mem:exp -> addr:exp -> exp -> endian -> size -> ('r,'a) state
      method eval_binop : binop -> exp -> exp -> ('r,'a) state
      method eval_unop  : unop -> exp -> ('r,'a) state
      method eval_cast  : cast -> nat1 -> exp -> ('r,'a) state
      method eval_let : var -> exp -> exp -> ('r,'a) state
      method eval_ite : cond:exp -> yes:exp -> no:exp -> ('r,'a) state
      method eval_concat : exp -> exp -> ('r,'a) state
      method eval_extract : nat1 -> nat1 -> exp -> ('r,'a) state
      method eval_unknown : string -> typ -> ('r,'a) state
    end

    class type virtual ['a,'r,'s] domain = object
      method private virtual undefined : ('r,'a) state
      method private virtual value_of_word : word -> ('r,'a) state
      method private virtual word_of_value : 'r -> (word option,'a) state
      method private virtual storage_of_value : 'r -> ('s option,'a) state
      method private virtual value_of_storage : 's -> ('r,'a) state
    end

    class type virtual ['a,'r,'s] eff = object
      method virtual lookup : var -> ('r,'a) state
      method virtual update : var -> 'r -> (unit,'a) state
      method virtual load   : 's -> addr -> ('r,'a) state
      method virtual store  : 's -> addr -> word -> ('r,'a) state
    end
  end

  module type S = sig
    type ('a,'e) state
    module State : T2 with type ('a,'e) t = ('a,'e) state

    class type ['a,'r] semantics = ['a,'r] Class(State).semantics
    class type virtual ['a,'r,'s] domain  = ['a,'r,'s] Class(State).domain
    class type virtual ['a,'r,'s] eff = ['a,'r,'s] Class(State).eff

    class virtual ['a,'r,'s] t : object
      inherit ['a,'r,'s] domain
      inherit ['a,'r,'s] eff
      inherit ['a,'r] semantics
      method type_error : type_error -> ('r,'a) state
      method division_by_zero : unit -> ('r,'a) state
    end
  end
end
