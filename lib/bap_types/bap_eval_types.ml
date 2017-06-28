open Core_kernel.Std
open Monads.Std
open Bap_common
open Bap_result
open Bap_bil
open Bap_type_error



module Eval = struct
  module T1(M : T1) = struct
    type 'a m = 'a M.t

    class type ['r] semantics = object
      method eval_exp : exp -> 'r m
      method eval_var : var -> 'r m
      method eval_int : word -> 'r m
      method eval_load : mem:exp -> addr:exp -> endian -> size -> 'r m
      method eval_store : mem:exp -> addr:exp -> exp -> endian -> size -> 'r m
      method eval_binop : binop -> exp -> exp -> 'r m
      method eval_unop  : unop -> exp -> 'r m
      method eval_cast  : cast -> nat1 -> exp -> 'r m
      method eval_let : var -> exp -> exp -> 'r m
      method eval_ite : cond:exp -> yes:exp -> no:exp -> 'r m
      method eval_concat : exp -> exp -> 'r m
      method eval_extract : nat1 -> nat1 -> exp -> 'r m
      method eval_unknown : string -> typ -> 'r m
    end

    class type virtual ['r,'s] domain = object
      method private virtual undefined : 'r m
      method private virtual value_of_word : word -> 'r m
      method private virtual word_of_value : 'r -> word option m
      method private virtual storage_of_value : 'r -> 's option m
    end

    class type virtual ['r,'s] eff = object
      method virtual lookup : var -> 'r m
      method virtual update : var -> 'r -> unit m
      method virtual load   : 's -> addr -> 'r m
      method virtual store  : 's -> addr -> word -> 'r m
    end
  end

  module T2(M : T2) = struct
    type ('a,'e) m = ('a,'e) M.t

    class type ['a,'r] semantics = object
      method eval_exp : exp -> ('r,'a) m
      method eval_var : var -> ('r,'a) m
      method eval_int : word -> ('r,'a) m
      method eval_load : mem:exp -> addr:exp -> endian -> size -> ('r,'a) m
      method eval_store : mem:exp -> addr:exp -> exp -> endian -> size -> ('r,'a) m
      method eval_binop : binop -> exp -> exp -> ('r,'a) m
      method eval_unop  : unop -> exp -> ('r,'a) m
      method eval_cast  : cast -> nat1 -> exp -> ('r,'a) m
      method eval_let : var -> exp -> exp -> ('r,'a) m
      method eval_ite : cond:exp -> yes:exp -> no:exp -> ('r,'a) m
      method eval_concat : exp -> exp -> ('r,'a) m
      method eval_extract : nat1 -> nat1 -> exp -> ('r,'a) m
      method eval_unknown : string -> typ -> ('r,'a) m
    end

    class type virtual ['a,'r,'s] domain = object
      method private virtual undefined : ('r,'a) m
      method private virtual value_of_word : word -> ('r,'a) m
      method private virtual word_of_value : 'r -> (word option,'a) m
      method private virtual storage_of_value : 'r -> ('s option,'a) m
    end

    class type virtual ['a,'r,'s] eff = object
      method virtual lookup : var -> ('r,'a) m
      method virtual update : var -> 'r -> (unit,'a) m
      method virtual load   : 's -> addr -> ('r,'a) m
      method virtual store  : 's -> addr -> word -> ('r,'a) m
    end
  end

  module type S = sig
    type 'a m
    module M : T1 with type 'a t = 'a m

    class type ['r] semantics = ['r] T1(M).semantics
    class type virtual ['r,'s] domain  = ['r,'s] T1(M).domain
    class type virtual ['r,'s] eff = ['r,'s] T1(M).eff

    class virtual ['r,'s] t : object
      inherit ['r,'s] domain
      inherit ['r,'s] eff
      inherit ['r] semantics
      method type_error : type_error -> 'r m
      method division_by_zero : unit -> 'r m
    end
  end

  module type S2 = sig
    type ('a,'e) m
    module M : T2 with type ('a,'e) t = ('a,'e) m

    class type ['a,'r] semantics = ['a,'r] T2(M).semantics
    class type virtual ['a,'r,'s] domain  = ['a,'r,'s] T2(M).domain
    class type virtual ['a,'r,'s] eff = ['a,'r,'s] T2(M).eff

    class virtual ['a,'r,'s] t : object
      inherit ['a,'r,'s] domain
      inherit ['a,'r,'s] eff
      inherit ['a,'r] semantics
      method type_error : type_error -> ('r,'a) m
      method division_by_zero : unit -> ('r,'a) m
    end
  end
end
