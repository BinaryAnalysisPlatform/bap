open Core_kernel.Std
open Bap.Std

module Iterator = Primus_iterator
module Random   = Primus_random


(** Generate values from a finite domain *)
module type Progress = sig
  type t
  val coverage : t -> float
end

(** Generate all possible values of a byte.

    The values are generated in the following order:

    [0, 255, 1, 254, ..., 256-n, n, ..., 129, 128],

    or, if a byte is treated as signed, then:

    [0,-1,1,-2,2,...,-n,n,-128,128]

  *)
module Bytes : sig
  include Iterator.Finite.S with type dom = int
  include Progress with type t := t
end = struct
  type t = Observe of int
  type dom = int

  let min = 0
  let max = 255

  let next = function
    | Observe 128 -> None
    | Observe n when n < 128 -> Some (Observe (256-(n+1)))
    | Observe n -> Some (Observe (256 - n))

  let value (Observe n) = n

  let coverage = function
    | Observe n when n > 128 -> float (256-n) /. 128.
    | Observe n -> float n /. 128.
end

let bytes = Iterator.Finite.create (module Bytes)


let uniform_coverage ~total ~trials =
  ~-.(expm1 (float trials *. log1p( ~-.(1. /. float total))))

module Uniform = struct
  module Memoryless(Rng : Random.S with type dom = int) = struct
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

  module With_memory(R : Random.S with type dom = int) = struct

    let permute_in_place xs rng =
      Seq.take (Iterator.enum R.t rng) (Array.length xs) |>
      Seq.map ~f:(fun i -> i mod Array.length xs) |>
      Seq.iteri ~f:(fun i j ->
          let z = xs.(i) in
          xs.(i) <- xs.(j);
          xs.(j) <- z)

    type t = {
      dom : int array;
      pos : int;
    }

    let observe {pos; dom} =
      if pos + 1 < Array.length dom
      then {pos = pos + 1; dom}
      else {pos; dom}

    let create dom rng =
      let dom = Array.copy dom in
      permute_in_place dom rng;
      observe {dom; pos=(-1)}

    let value {dom; pos} = dom.(pos)
    let coverage {dom;pos} =
      (float pos +. 1.) /. float (Array.length dom)
  end
end
