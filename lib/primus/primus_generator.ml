open Core_kernel.Std
open Bap.Std

open Primus_types
include Primus_generator_types

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
  module Byte = struct
    module type S = Byte
    module Make(Rng : Random.S with type dom = int)
      : S with type rng = Rng.t
    = struct
      type dom = int
      type rng = Rng.t
      type t = Rng.t

      let min = 0
      let max = 255
      let size = max - min + 1
      let create = ident
      let next = Rng.next
      let value rng = Rng.value rng mod size
    end
    module Basic = Make(Random.LCG)
    include (Basic : S with type rng = Random.LCG.t)
  end

  module type S = sig
    include Iterator.Infinite.S with type dom = int
    include Progress with type t := t
    type rng
    val create : rng -> t
  end

  module Memoryless = struct
    module type S = S
    module Make(Rng : Random.S with type dom = int)
      : S with type rng := Rng.t
    = struct
      type dom = int
      type t = {
        rng : Rng.t;
        trials : int;
      }

      let min = 0
      let max = 255
      let size = 256

      let create rng = {rng; trials = 0}

      let next t = {rng = Rng.next t.rng; trials = t.trials + 1}
      let value {rng} = Rng.value rng mod size
      let coverage {trials} =
        uniform_coverage ~total:size ~trials
    end
  end

  module With_memory = struct
    module type S = S
    module Make(R : Random.S with type dom = int)
      : S with type rng := R.t
    = struct
      type dom = int
      type rng = R.t
      type t = {
        dom : int array;
        pos : int;
      }
      let min = 0
      let max = 255

      let permute_in_place xs rng =
        Seq.take (Iterator.enum R.t rng) (Array.length xs) |>
        Seq.map ~f:(fun i -> i mod Array.length xs) |>
        Seq.iteri ~f:(fun i j ->
            let z = xs.(i) in
            xs.(i) <- xs.(j);
            xs.(j) <- z)

      let next {pos; dom} =
        if pos + 1 < Array.length dom
        then {pos = pos + 1; dom}
        else {pos = 0; dom}

      let create rng =
        let dom = Array.init 256 ident in
        permute_in_place dom rng;
        next {dom; pos=(-1)}

      let value {dom; pos} = dom.(pos)
      let coverage {dom;pos} =
        (float pos +. 1.) /. float (Array.length dom)
    end
  end
end

module Make(Machine : Machine) = struct
  open Machine.Syntax
  type 'e context = 'e constraint 'e = #Context.t
  type t = {next : 'e . unit -> (int,'e context) Machine.t}
  type policy = [`random of t option | `static of word]


  let with_init (type rng)
      (module Rng : Iterator.Infinite.S
        with type t = rng and type dom = int) init =
    let state = Machine.Local.create ~name:"rng" init in {
      next = fun () ->
        Machine.Local.get state >>= fun rng ->
        let x = Rng.value rng in
        Machine.Local.put state (Rng.next rng) >>= fun () ->
        Machine.return x
    }

  let create rng_t rng = with_init rng_t (fun _ -> rng)

  let lcg seed =
    with_init (module Random.LCG) (fun _ -> Random.LCG.create seed)

  let byte seed =
    with_init (module Uniform.Byte) (fun _ ->
        Uniform.Byte.create (Random.LCG.create seed))

  let next t = t.next ()

  module Seeded = struct
    let create rng =
      Machine.current () >>| fun id ->
      rng (Machine.Id.hash id)

    let byte () = create byte
    let lcg () = create lcg
  end
end
