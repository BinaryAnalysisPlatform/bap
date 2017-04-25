open Core_kernel.Std


(** [next ~is_stop ~read off] reads characters starting with the
    offset [off], using the [read] function to map offsets to
    characters, until the [is_stop] character occurs, or the [read]
    function returns [None].

    Postcondition:
    forall [c] in [result], [not (is_stop c)]

*)
val next : ?is_stop:(char -> bool) -> read:(int -> char option) -> int -> string



(** [scan ~is_stop ~read off] applies [next ~is_stop ~read] for all
    offsets that are bigger than [off] until [read off] is not [None].
    Returns a sequence of non-empty strings, paired with their
    corresponding offsets in data. *)
val run : ?is_stop:(char -> bool) -> read:(int -> char option) ->
  int -> (int * string) Sequence.t
