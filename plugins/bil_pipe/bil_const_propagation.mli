open Bap.Std

(** [propagate_consts bil] implements a simple worklist
    algorithm for consts propagation. The only exception are
    loops and let-expression, which are not participate in propagation. *)
val propagate_consts: bil -> bil

val propagate_copy  : bil -> bil
