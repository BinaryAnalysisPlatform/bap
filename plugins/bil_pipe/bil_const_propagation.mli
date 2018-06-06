open Bap.Std

(** [propagate_consts bil] implements a simple worklist
    algorithm for consts propagation. The only exception are
    loops and let-expression, which are not participate in propagation. *)
val propagate_consts: bil -> bil

(** [propagate_copy bil] performs a copy-propagation in [bil].
    if [virtual_only] is true (default) then do copy propagation
    only for virtual variables *)
val propagate_copy  : ?virtual_only:bool -> bil -> bil
