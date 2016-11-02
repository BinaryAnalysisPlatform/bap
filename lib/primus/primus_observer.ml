open Core_kernel.Std
open Bap.Std

module Context = Primus_context

module Rng = Primus_random

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

module Byte = struct
  module Det = struct
    type _ state =
      | Empty : [>] state
      | Observe  : int -> [`Full] state
      | Observed : int -> [`Full] state

    let observe : type a. a state -> [`Full] state = function
      | Empty -> Observe 0
      | Observe 128 -> Observed 128
      | Observe n when n < 128 -> Observe (256-(n+1))
      | Observe n -> Observe (256 - n)
      | Observed n -> Observed n

    let value (Observed n | Observe n) = n

    let coverage = function
      | Empty -> 0.
      | Observed _  -> 1.0
      | Observe n when n > 128 -> float (256-n) /. 128.
      | Observe n -> float n /. 128.

    let observed = function
      | Observed _ -> true
      | _ -> false

    let enum =
      Sequence.unfold ~init:Empty ~f:(fun s ->
          match observe s with
          | Observed n -> None
          | s -> Some (value s,s))
  end
end


let uniform_coverage ~total ~trials =
  ~-.(expm1 (float trials *. log1p( ~-.(1. /. float total))))

module Uniform = struct
  module Memoryless(Rng : Rng.S with type dom = int) = struct
    type t = {
      rng : Rng.t;
      dom : int array;
      trials : int;
    }

    let create dom rng = {rng; dom; trials = 0}

    let observe t = {t with rng = Rng.next t.rng; trials = t.trials + 1}

    let value {dom; rng} = dom.(Rng.value rng mod Array.length dom)

    let coverage {dom; trials} =
      uniform_coverage ~total:(Array.length dom) ~trials

  end



  module With_memory = struct
    type state = One of int | Cons of int * state

    let of_array xs =
      let rec loop i acc =
        if i < 0 then acc
        else loop (i-1) (Cons (xs.(i),acc)) in
      match Array.length xs with
      | 0 -> invalid_arg "domain must be non empty"
      | 1 -> One xs.(0)
      | n -> loop (n-1) (One xs.(n-1))



  end

end
