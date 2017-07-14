open Core_kernel.Std
open Bap.Std
open Bap_primus_types

val enter_term : tid observation
val leave_term : tid observation

val enter_exp : exp observation
val leave_exp : exp observation
val enter_pos : pos observation
val leave_pos : pos observation

val enter_top : program term observation
val enter_sub : sub term observation
val enter_arg : arg term observation
val enter_blk : blk term observation
val enter_phi : phi term observation
val enter_def : def term observation
val enter_jmp : jmp term observation

val leave_top : program term observation
val leave_sub : sub term observation
val leave_arg : arg term observation
val leave_blk : blk term observation
val leave_phi : phi term observation
val leave_def : def term observation
val leave_jmp : jmp term observation

val halting : unit observation

(* todo, add events that correspond to the interpreter methods,

   e.g.,
   - bop binop x y
   - uop unop  x
   - cast hi lo x
   - load ..
   - save ..

   would it be possible to provide this events without an allocation?

   1. rely on the inlining and implement observation with a thunk,
      e.g., [let provide obs = if obs.requested then obs.invoke ()].
      If the provide function will be inlined, it will remove
      allocation.
      Actually, even without a thunk, we should use the if statement, this
      might save us from an allocation (probably with even higher
      probability).

   2. An observer may take a handler, that will return operands. But
      how we can implement this handler without performing an
      allocation? We can store values in a stack, and the handler can
      be actually a state with a restricted interface. But the problem
      here is that: (a) storing values in a stack will be an
      allocation already, and (b) extracting a value from a state
      monad will also lead to allocations.

 *)

type exn += Halt

module Make (Machine : Machine) : sig
  type 'a m = 'a Machine.t
  val halt : never_returns m
  val pos : pos m
  val sub : sub term -> unit m
  val blk : blk term -> unit m
  val get : var -> value m
  val set : var -> value -> unit m
  val bop : binop -> value -> value -> value m
  val uop : unop -> value -> value m
  val cast : hi:int -> lo:int -> value -> value m
  val load : value -> value m
  val save : value -> value -> unit m
end


module Init (Machine : Machine) : sig
  val run : unit -> unit Machine.t
end
