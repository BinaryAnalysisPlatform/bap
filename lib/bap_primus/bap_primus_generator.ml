open Core_kernel
open Bap_knowledge

open Bap_primus_types
module Machine = Bap_primus_machine
module Word = Bitvec.M64

open Machine.Syntax

type descr = {
  width : int;
  min : word;
  max : word;
}

type state = {
  states : word Int.Map.t;
}

let generators = Bap_primus_machine.State.declare
    ~name:"rng-states"
    ~uuid:"7e81d5ae-46a2-42ff-918f-96c0c2dc95e3" @@
  Knowledge.return {states = Int.Map.empty}



type t =
  | Static of word
  | Random of {
      ident : int;
      descr : descr;
      seed :  word;
    }
  | Custom of {
      next : word Machine.t;
      descr : descr;
    }

let sexp_of_descr {min; max} = Sexp.List [
    Sexp.Atom "random";
    sexp_of_word min;
    sexp_of_word max;
  ]

let rec sexp_of_t = function
  | Static x -> Sexp.(List [Atom "static"; sexp_of_word x])
  | Random {descr} | Custom {descr} -> sexp_of_descr descr

module LCG : sig
  val size : int
  val next : word -> word
  val seed : word
end = struct
  let a = Word.int64 2862933555777941757L
  let c = Word.int64 3037000493L
  let next x = Word.(a * x + c)
  let size = 64
  let seed = Word.int64 42L
end

let static value = Static value
let custom ~min ~max width next = Custom {
    next;
    descr = {min; max; width}
  }

let random ?min ?max ?(seed=LCG.seed) width = Random {
    ident = 0;
    seed;
    descr = {
      min = Option.value min ~default:Word.zero;
      max = Option.value max ~default:Bitvec.(ones mod modulus width);
      width;
    }
  }

type outcome = {
  state : word;
  value : word;
}

let random_word ~state ~width =
  assert (width > 0);
  let rec loop left state =
    let state = LCG.next state in
    let width = min LCG.size left in
    let value = Bitvec.extract ~hi:(width-1) ~lo:0 state in
    if left = width then {value; state}
    else
      let next = loop (left - width) state in {
        value = Bitvec.append width (left - width) value next.value;
        state
      } in
  loop width (Bitvec.extract ~hi:(LCG.size-1) ~lo:0 state)

let rec next = function
  | Static word -> Machine.return word
  | Custom {next} -> next
  | Random {seed; ident; descr={width; min=x; max=y}} ->
    Machine.Local.get generators >>= fun {states} ->
    let state = if ident = 0
      then seed
      else Map.find_exn states ident in
    let {value; state} = random_word ~state ~width in
    let m = Bitvec.modulus width in
    let range = Bitvec.(succ ((y - x) mod m) mod m) in
    let value = Bitvec.(((x + value) mod m % range) mod m) in
    let ident = if ident <> 0 then ident
      else match Map.max_elt states with
        | None -> 1
        | Some (k,_) -> k+1 in
    let states = Map.set states ~key:ident ~data:state in
    Machine.Local.put generators {states} >>| fun () ->
    value
