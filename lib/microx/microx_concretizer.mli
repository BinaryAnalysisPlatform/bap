open Bap.Std

[@@@warning "-D"]

type policy = [
  | `Random
  | `Fixed of int64 | `Interval of int64 * int64 ]
[@@deriving sexp_of]


(** expression interpreter that never halts due to an unknown value.

    It can be used as mixin class for other interpreters.

    @param memory returns a byte value at the given address, if known
    @param lookup returns a value of a given variable, if known
    @param random_seed if non [None] the the value is used to
           initialize the pseudo random generator.
    @param reg_policy a policy for concretizing register values;
    @param mem_policy a policy for concretizing heap values.
*)
class ['a] main :
  ?memory:(addr -> word option) ->
  ?lookup:(var -> word option) ->
  ?random_seed:int ->
  ?reg_policy:policy ->
  ?mem_policy:policy -> unit -> ['a] expi
