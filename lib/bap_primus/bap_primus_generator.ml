open Core_kernel
open Bap.Std

open Bap_primus_types
open Bap_primus_generator_types

module Iterator = Bap_primus_iterator


let uniform_coverage ~total ~trials =
  ~-.(Float.expm1 (float trials *. Float.log1p( ~-.(1. /. float total))))



let generators : Univ_map.t state = Bap_primus_machine.State.declare
    ~name:"rng-states"
    ~uuid:"7e81d5ae-46a2-42ff-918f-96c0c2dc95e3"
    (fun _ -> Univ_map.empty)


module States = Univ_map.With_default

type 'a gen = {
  state : 'a States.Key.t;
  next : 'a -> 'a;
  value : 'a -> int;
  min : int;
  max : int;
}

type t =
  | Gen : 'a gen -> t
  | Wait : (int -> t) -> t
  | Static : int -> t

let rec sexp_of_t = function
  | Static x -> Sexp.(List [Atom "static"; sexp_of_int x])
  | Gen {min;max} -> Sexp.List [
      Sexp.Atom "interval";
      sexp_of_int min;
      sexp_of_int max;
    ]
  | Wait create -> Sexp.List [Sexp.Atom "project-0"; sexp_of_t (create 0)]

let create (type rng)
    (module Rng : Iterator.Infinite.S
      with type t = rng and type dom = int) init =
  let state = States.Key.create
      ~default:init ~name:"rng-state" sexp_of_opaque in
  Gen {state; next=Rng.next; value=Rng.value; min=Rng.min; max=Rng.max}

let unfold (type gen)
    ?(min=Int.min_value)
    ?(max=Int.max_value)
    ?(seed=0)
    ~f init  =
  let module Gen = struct
    type t = gen * int
    type dom = int
    let min = min
    let max = max
    let next = f
    let value = snd
  end in
  create (module Gen) (init,seed)

let static value = Static value


module Random = struct
  open Bap_primus_random

  let lcg ?(min=LCG.min) ?(max=LCG.max) seed =
    let lcg = LCG.create seed in
    unfold ~min ~max ~f:(fun (lcg,value) ->
        let x = LCG.value lcg in
        (LCG.next lcg, min + x mod (max-min+1))) lcg

  let byte seed = lcg ~min:0 ~max:255 seed

  module Seeded = struct
    let create make = Wait make
    let lcg ?min ?max () = Wait (fun seed -> lcg ?min ?max seed)
    let byte = Wait byte
  end
end

module Make(Machine : Machine) = struct
  open Machine.Syntax
  let rec next = function
    | Gen {state; next; value} ->
      Machine.Local.get generators >>= fun states ->
      let gen = States.find states state in
      let states = States.set states state (next gen) in
      Machine.Local.put generators states >>| fun () ->
      value gen
    | Wait create ->
      Machine.current () >>= fun id ->
      next (create (Machine.Id.hash id))
    | Static n -> Machine.return n
end
