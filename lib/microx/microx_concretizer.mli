open Bap.Std

type policy = [`Random | `Fixed of int64 | `Interval of int64 * int64 ]
  [@@deriving sexp_of]

class ['a] main :
  ?memory:(addr -> word option) ->
  ?lookup:(var -> word option) ->
  ?random_seed:int ->
  ?reg_policy:policy ->
  ?mem_policy:policy -> unit -> ['a] expi
