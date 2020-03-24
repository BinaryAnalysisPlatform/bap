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

type mode =
  | Static : int -> mode
  | Ready : 'a ready -> mode
  | Wait : 'a wait -> mode
and t = {
  size : int;
  mode : mode;
}

let width x = x.size

let rec sexp_of_t = function
  | {mode=Static x} -> Sexp.(List [Atom "static"; sexp_of_int x])
  | _ -> Sexp.Atom "<generator>"

let make ?(width=8) key init iter = {
  size = width;
  mode = Ready {key; gen={iter; self=init}}
}

let create ?width iter init =
  let state = States.Key.create
      ~name:"rng-state" sexp_of_opaque in
  make ?width state init iter

let unfold (type gen)
    ?width
    ?(min=Int.min_value)
    ?(max=Int.max_value)
    ?(seed=0)
    ~f init =
  let module Gen = struct
    type t = gen * int
    type dom = int
    let min = min
    let max = max
    let next = f
    let value = snd
  end in
  create ?width (module Gen) (init,seed)

let static ?(width=8)value = {mode=Static value; size=width}


module Random = struct
  open Bap_primus_random

  let lcg_key : (LCG.t * int) gen States.Key.t =
    States.Key.create ~name:"linear-congruent-generator"
      sexp_of_opaque

  let lcg ?width ?(min=LCG.min) ?(max=LCG.max) seed =
    let next (gen,_) =
      let gen = LCG.next gen in
      let r = Int.abs @@ LCG.value gen in
      let x = min + r mod (max-min+1) in
      gen,x in
    let value = snd in
    let init = next (LCG.create seed,0) in
    make ?width lcg_key init (module struct
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
    let unpack_make ?(width=8) key make =
      let init seed  = match make seed with
        | {mode=Wait _|Static _} ->
          failwith "Generator.Seeded: invalid initializer"
        | {mode=Ready g} -> match cast_gen key g with
          | Some g -> g
          | None -> invalid_arg "Seeded.create changed its type" in
      {mode = Wait {key; init}; size = width}

    let create ?width make = match make 0 with
      | {mode=Ready {key}} -> unpack_make ?width key make
      | _ -> invalid_arg "Seeded.create must always create \
                          an iterator of the same type"

    let lcg ?width ?min ?max () = unpack_make ?width lcg_key (fun seed ->
        lcg ?width ?min ?max seed)

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

  let rec next gen = match gen.mode with
    | Static n -> Machine.return n
    | Ready {key; gen} -> call key gen
    | Wait {key; init} ->
      Machine.Local.get generators >>= fun states ->
      match States.find states key with
      | None ->
        Machine.current () >>= fun id ->
        call key (init (Machine.Id.hash id mod 4096))
      | Some iter -> call key iter

  let word gen width =
    let word = Word.of_int ~width:gen.size in
    assert (width > 0);
    let rec loop x =
      if Word.bitwidth x >= width
      then Machine.return (Word.extract_exn ~hi:(width-1) x)
      else next gen >>= fun y -> loop (Word.concat x (word y)) in
    next gen >>| word >>= loop
end
