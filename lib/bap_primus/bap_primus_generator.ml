open Core_kernel
open Bap.Std

open Bap_primus_types
open Bap_primus_generator_types

module Iterator = Bap_primus_iterator

type 'a iface =
  (module Iterator.Infinite.S with type dom = Bitvec.t
                               and type t = 'a)

type iterator = Iter : {
    ctrl : 'a iface;
    self : 'a;
    seed : int -> 'a
  } -> iterator

type iterators = {
  iterators : iterator Int.Map.t;
  salt : int;
}

let iterators = Bap_primus_machine.State.declare
    ~name:"iterators"
    ~uuid:"9927004d-fe57-4de9-8705-c3d6862238cf" @@ fun _ -> {
    iterators = Int.Map.empty;
    salt = 1;
  }

type constructor =
  | Const of iterator
  | Seeded of (int -> iterator)

type t = {
  size : int;
  init : constructor;
  id : int;
}

let last_id = ref 0

let width x = x.size
let sexp_of_t {id} = Sexp.List [
    Atom "generator";
    sexp_of_int id;
  ]

let of_iterator (type dom) (type t)
    ?(width=8)
    ?seed
    ~to_bitvec
    (module Iter : Iterator.Infinite.S
      with type t = t
       and type dom = dom) self =
  incr last_id;
  let module Iface = struct
    type t = Iter.t
    type dom = Bitvec.t
    let min = to_bitvec Iter.min
    let max = to_bitvec Iter.max
    let value self = to_bitvec @@ Iter.value self
    let next self = Iter.next self
  end in {
    size=width;
    id = !last_id;
    init = Const (Iter {
        ctrl = (module Iface);
        self;
        seed = match seed with
          | Some seed -> seed
          | None -> fun _ -> self
      })
  }

let create ?(width=8) iter init =
  let m = Bitvec.modulus width in
  of_iterator ~width iter init
    ~to_bitvec:(fun x -> Bitvec.(int x mod m))

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

let static ?(width=8) value =
  let value = Bitvec.(int value mod modulus width) in
  of_iterator ~width ~to_bitvec:ident (module struct
    type t = Bitvec.t
    type dom = Bitvec.t
    let min = value
    let max = value
    let next _ = value
    let value _ = value
  end) value


module Random = struct
  module MCG = Bap_primus_random.MCG
  let create_lcg ~don't_seed ?(width=8) ?min ?max start =
    let module Gen = (val MCG.create_small ?min ?max width) in
    let seed = if don't_seed
      then fun _ -> Gen.create start
      else Gen.create in
    of_iterator ~width ~to_bitvec:ident ~seed
      (module Gen) (Gen.create start)

  let lcg = create_lcg ~don't_seed:true
  let byte seed = lcg ~min:0 ~max:255 seed

  module Seeded = struct
    let create ?(width=8) init =
      incr last_id;
      let init seed = match init seed with
        | {init=Const iter} -> iter
        | {init=Seeded init} -> init seed in
      {size=width; init = Seeded init; id = !last_id}

    let lcg ?width ?min ?max () =
      create_lcg ~don't_seed:false ?width ?min ?max 0

    let byte = lcg ()
  end

end


module Make(Machine : Machine) = struct
  open Machine.Syntax

  let rec call gen =
    Machine.Local.get iterators >>= fun {iterators=iters; salt} ->
    match Map.find iters gen.id with
    | Some Iter {ctrl=(module Iter); self; seed} ->
      let value = Iter.value self in
      let next = Iter {
          ctrl = (module Iter);
          self = Iter.next self;
          seed
        } in
      Machine.Local.put iterators {
        iterators = Map.set iters gen.id next;
        salt;
      } >>| fun () ->
      value
    | None ->
      Machine.current () >>= fun id ->
      let seed = Machine.Id.hash id + salt in
      match gen.init with
      | Const Iter it ->
        let iter = Iter {it with self = it.seed seed} in
        Machine.Local.put iterators {
          iterators = Map.set iters gen.id iter;
          salt = salt + 1;
        } >>= fun () ->
        call gen
      | Seeded init ->
        Machine.Local.put iterators {
          iterators = Map.set iters gen.id (init seed);
          salt;
        } >>= fun () ->
        call gen

  let next gen = call gen >>| fun x ->
    if Bitvec.fits_int x
    then Bitvec.to_int x
    else Int.max_value

  let word gen width =
    let rec loop built x =
      if built >= width
      then Machine.return (Word.create x width)
      else
        call gen >>= fun y ->
        loop (built+gen.size) @@
        Bitvec.append built gen.size x y in
    call gen >>= loop gen.size
end
