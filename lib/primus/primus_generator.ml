open Core_kernel.Std
open Bap.Std

open Primus_types
open Primus_generator_types

module Iterator = Primus_iterator


let uniform_coverage ~total ~trials =
  ~-.(expm1 (float trials *. log1p( ~-.(1. /. float total))))



let generators : Univ_map.t state = Primus_machine.State.declare
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
  | Gen {min;max} -> Sexp.Atom (sprintf "(%d,%d)" min max)
  | Wait create -> Sexp.List [Sexp.Atom "create 0:"; sexp_of_t (create 0)]

let create (type rng)
    (module Rng : Iterator.Infinite.S
      with type t = rng and type dom = int) init =
  let state = States.Key.create
      ~default:init ~name:"rng-state" sexp_of_opaque in
  Gen {state; next=Rng.next; value=Rng.value; min=Rng.min; max=Rng.max}

let static value = Static value


module Random = struct
  open Primus_random
  let lcg seed =
    create (module LCG) (LCG.create seed)

  let byte seed =
    create (module Byte) (Byte.create (LCG.create seed))

  module Seeded = struct
    let create make = Wait make
    let lcg = Wait lcg
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
