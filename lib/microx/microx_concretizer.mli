open Bap.Std

type policy = [`Random | `Fixed of int64 | `Interval of int64 * int64 ]

class ['a] main :
  ?memory:(addr -> word option) ->
  ?policy:policy -> unit -> ['a] expi
