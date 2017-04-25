open Bap_result
open Bap_common
open Bap_bil
open Bap_type_error


module Context = struct
  class type t = object('s)
    inherit Bap_context.t
    method create_undefined : 's * result
    method create_word : word -> 's * result
    method create_storage : storage -> 's * result
  end
end

module Expi = struct
  module type S = sig
    type ('a,'e) state
    type 'a u = (unit,'a) state
    type 'a r = (result,'a) state

    class ['a] t : object
      constraint 'a = #Context.t
      method empty  : storage
      method lookup : var -> 'a r
      method update : var -> result -> 'a u
      method load   : storage -> addr -> 'a r
      method store  : storage -> addr -> word -> 'a r

      method type_error : type_error -> 'a r
      method division_by_zero : unit -> 'a r
      method undefined_addr : addr -> 'a r
      method undefined_var  : var  -> 'a r

      method eval_exp : exp -> 'a r
      method eval_var : var -> 'a r
      method eval_int : word -> 'a r
      method eval_load : mem:exp -> addr:exp -> endian -> size -> 'a r
      method eval_store : mem:exp -> addr:exp -> exp -> endian -> size -> 'a r
      method eval_binop : binop -> exp -> exp -> 'a r
      method eval_unop  : unop -> exp -> 'a r
      method eval_cast  : cast -> nat1 -> exp -> 'a r
      method eval_let : var -> exp -> exp -> 'a r
      method eval_ite : cond:exp -> yes:exp -> no:exp -> 'a r
      method eval_concat : exp -> exp -> 'a r
      method eval_extract : nat1 -> nat1 -> exp -> 'a r
      method eval_unknown : string -> typ -> 'a r
    end
  end
end
