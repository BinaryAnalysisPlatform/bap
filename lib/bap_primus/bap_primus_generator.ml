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

module States = Univ_map

module type Iter =
  Iterator.Infinite.S with type dom = int

type 'a iter = (module Iter with type t = 'a)

type 'a gen = {
  iter : 'a iter;
  self : 'a;
}

type 'a key = 'a gen States.Key.t

type 'a ready = {
  key : 'a key;
  gen : 'a gen;
}

type 'a wait = {
  key : 'a key;
  init : int -> 'a gen
}

type t =
  | Static : int -> t
  | Ready : 'a ready -> t
  | Wait : 'a wait -> t

let rec sexp_of_t = function
  | Static x -> Sexp.(List [Atom "static"; sexp_of_int x])
  | _ -> Sexp.Atom "<generator>"

let make key init iter = Ready {key; gen={iter; self=init}}

let create iter init =
  let state = States.Key.create
      ~name:"rng-state" sexp_of_opaque in
  make state init iter

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

  let lcg_key : (LCG.t * int) gen States.Key.t =
    States.Key.create ~name:"linear-congruent-generator"
      sexp_of_opaque

  let lcg ?(min=LCG.min) ?(max=LCG.max) seed =
    let next (gen,_) =
      let gen = LCG.next gen in
      let x = min + LCG.value gen mod (max-min+1) in
      gen,x in
    let value = snd in
    let init = next (LCG.create seed,0) in
    make lcg_key init (module struct
      type t = LCG.t * int
      type dom = int
      let min = min
      let max = max
      let next = next
      let value = value
    end)

  let byte seed = lcg ~min:0 ~max:255 seed

  let cast_gen : type a b. a key -> b ready -> a gen option =
    fun k1 {key=k2; gen} -> match States.Key.same_witness k1 k2 with
      | None -> None
      | Some Type_equal.T -> Some gen


  module Seeded = struct
    let unpack_make key make =
      let init seed  = match make seed with
        | Wait _
        | Static _ -> failwith "Generator.Seeded: invalid initializer"
        | Ready g -> match cast_gen key g with
          | Some g -> g
          | None -> invalid_arg "Seeded.create changed its type" in
      Wait {key; init}

    let create make = match make 0 with
      | Ready {key} -> unpack_make key make
      | _ -> invalid_arg "Seeded.create must always create \
                          an iterator of the same type"

    let lcg ?min ?max () = unpack_make lcg_key (fun seed ->
        lcg ?min ?max seed)

    let byte = lcg ~min:0 ~max:255 ()
  end
end


module Make(Machine : Machine) = struct
  open Machine.Syntax

  let call (type a) (state : a key) ({iter; self} : a gen) =
    let module Iter : Iter with type t = a = (val iter) in
    Machine.Local.get generators >>= fun states ->
    let self = match States.find states state with
      | None -> self
      | Some {self} -> self in
    let iter = {
      iter;
      self = Iter.next self;
    } in
    let states = States.set states state iter in
    Machine.Local.put generators states >>| fun () ->
    Iter.value iter.self

  let rec next = function
    | Static n -> Machine.return n
    | Ready {key; gen} -> call key gen
    | Wait {key; init} ->
      Machine.Local.get generators >>= fun states ->
      match States.find states key with
      | None ->
        Machine.current () >>= fun id ->
        call key (init (Machine.Id.hash id))
      | Some iter -> call key iter
end
