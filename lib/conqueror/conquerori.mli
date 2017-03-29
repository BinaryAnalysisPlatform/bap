open Bap.Std
open Monads.Std

[@@@deprecated "use Biri.Microx.t instead"]

(** Deprecated microx intepreter.

    This module implements a variant of a microexecution interpreter,
    and is kept only for the backward compatibility. It is not supported
    anymore, may contain bugs, and will be removed in BAP.2.0.0.

    Please, use the new Primus framework for the microexecution.
 *)

class context : ?main : sub term -> program term ->  object('s)
    inherit Expi.context
    method program : program term
    method main : sub term option
    method enter_term : 't 'p . ('p,'t) cls -> 't term -> 's
    method leave_term : 't 'p . ('p,'t) cls -> 't term -> 's
    method set_next : tid option -> 's
    method next : tid option
    method curr : tid
  end

module type S = sig

  type ('a,'e) state
  type 'a u = (unit,'a) state
  type 'a r = (Bil.result,'a) state

  module Expi : Expi.S with type ('a,'e) state = ('a,'e) state

  class ['a] t : object
    constraint 'a = #context
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

module Make(M : Monad.State.S2) : S with type ('a,'e) state = ('a,'e) M.t
include S with type ('a,'e) state = ('a,'e) Monad.State.t
