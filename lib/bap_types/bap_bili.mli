open Core_kernel.Std
open Bap_result
open Bap_common
open Bap_bil

class context : object('s)
  inherit Bap_expi.context
  method pc : value
  method with_pc : value -> 's
end

class ['a] t : object
  constraint 'a = #context
  inherit ['a] Bap_expi.t
  method eval : stmt list -> 'a u
  method eval_stmt : stmt -> 'a u
  method eval_move : var -> exp -> 'a u
  method eval_jmp : exp -> 'a u
  method eval_while : cond:exp -> body:stmt list -> 'a u
  method eval_if : cond:exp -> yes:stmt list -> no:stmt list -> 'a u
  method eval_cpuexn : int -> 'a u
  method eval_special : string -> 'a u
end
