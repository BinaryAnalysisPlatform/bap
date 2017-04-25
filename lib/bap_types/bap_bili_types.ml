open Bap_bil
open Bap_expi_types
open Bap_result

module Context = struct
  class type t = object('s)
    inherit Bap_expi.context
    method pc : value
    method with_pc : value -> 's
  end
end

module Bili = struct
  module type S = sig

    type ('a,'e) state
    type 'a u = (unit,'a) state
    type 'a r = (result,'a) state

    module Expi : Expi.S with type ('a,'e) state = ('a,'e) state

    class ['a] t : object
      constraint 'a = #Context.t
      inherit ['a] Expi.t
      method eval : stmt list -> 'a u
      method eval_stmt : stmt -> 'a u
      method eval_move : var -> exp -> 'a u
      method eval_jmp : exp -> 'a u
      method eval_while : cond:exp -> body:stmt list -> 'a u
      method eval_if : cond:exp -> yes:stmt list -> no:stmt list -> 'a u
      method eval_cpuexn : int -> 'a u
      method eval_special : string -> 'a u
    end
  end
end
