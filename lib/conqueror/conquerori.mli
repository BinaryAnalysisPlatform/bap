open Bap_bil
open Bap_ir
open Bap_result

class context : ?main : sub term -> program term ->  object('s)
    inherit Bap_expi.context
    method program : program term
    method main : sub term option
    method enter_term : 't 'p . ('p,'t) cls -> 't term -> 's
    method leave_term : 't 'p . ('p,'t) cls -> 't term -> 's
    method set_next : tid option -> 's
    method next : tid option
    method curr : tid
  end

class ['a] t : object
  constraint 'a = #context
  inherit ['a] Bap_expi.t

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
