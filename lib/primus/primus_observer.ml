open Core_kernel.Std
open Bap.Std

module Context = Primus_context
module Random = Primus_random
module Iterator = Primus_iterator


module type S = sig
  type ('a,'e) m constraint 'e = #Context.t
  type cell
  val step : ('p,'t) cls -> 't term -> (unit,'e) m

  (* each call to observe creates a new value.
     an observer must not create new contexts (we should enforce this
     by providing only a deterministic machine interface).

     How to tell, that observer has observed all possible values.
 *)
  val observe : cell -> (word,'e) m

  (** [coverage cell] is a value between [0] and [1], that shows a
      percentage (deterministically or probobalistically) of an area
      of the value domain, that was observed.


      Alternatively:

      val entropy : cell -> (float,'e) m
*)
  val coverage : cell -> (float,'e) m


  (** [observed cell] is true if the cell is deterministically
      observed, i.e., when all possible values were observed.  It is
      possible that the coverage is equal [1.0], but [observed] is
      false. For example, if the observer is a random distribution
      observer.
  *)
  val observed : cell -> (bool,'e) m
end
