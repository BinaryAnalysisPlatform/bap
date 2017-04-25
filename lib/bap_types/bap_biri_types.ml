open Bap_bil
open Bap_ir
open Bap_result
open Bap_expi_types

module Context = struct
  class type t = object('s)
    inherit Bap_expi.context
    method program : program term
    method main : sub term option
    method trace : tid list
    method enter_term : tid -> 's
    method set_next : tid option -> 's
    method next : tid option
  end
end


module Biri = struct
  module type S = sig

    type ('a,'e) state
    type 'a u = (unit,'a) state
    type 'a r = (result,'a) state

    module Expi : Expi.S with type ('a,'e) state = ('a,'e) state

    class ['a] t : object
      constraint 'a = #Context.t

      inherit ['a] Expi.t

      method enter_term : 't 'p . ('p,'t) cls -> 't term -> 'a u

      method eval : 't 'p. ('p,'t) cls -> 't term -> 'a u

      method leave_term : 't 'p . ('p,'t) cls -> 't term -> 'a u

      method eval_sub : sub term -> 'a u
      method eval_blk : blk term -> 'a u
      method eval_arg : arg term -> 'a u
      method eval_def : def term -> 'a u
      method eval_phi : phi term -> 'a u
      method eval_jmp : jmp term -> 'a u

      method eval_goto : label -> 'a u
      method eval_call : call -> 'a u
      method eval_ret  : label -> 'a u
      method eval_exn  : int -> tid -> 'a u

      method eval_direct : tid -> 'a u
      method eval_indirect : exp -> 'a u
    end
  end
end
