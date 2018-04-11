open Core_kernel
open Bap_result
open Bap_common
open Bap_bil
open Bap_type_error
open Bap_eval_types


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

    module M : T2 with type ('a,'e) t = ('a,'e) state

    module Eval : Eval.S2 with type ('a,'e) m := ('a,'e) state
                           and module M := M

    class ['a] t : object
      constraint 'a = #Context.t
      inherit ['a,result] Eval.semantics
      method empty  : storage
      method lookup : var -> 'a r
      method update : var -> result -> 'a u
      method load   : storage -> addr -> 'a r
      method store  : storage -> addr -> word -> 'a r
      method undefined_addr : addr -> 'a r
      method undefined_var  : var  -> 'a r
      method type_error : type_error -> 'a r
      method division_by_zero : unit -> 'a r
    end
  end
end
