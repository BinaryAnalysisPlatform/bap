open Core_kernel
open Bap_strings.Std
open Format

open Bap_knowledge
open Bap_core_theory

open Bap_primus_types

module Observation = Bap_primus_observation
module Machine = Bap_primus_machine
open Machine.Syntax


module Id = Int63

type id = Id.t [@@deriving bin_io, compare, sexp]


let compare_value x y = Word.compare x.value y.value
type t = value [@@deriving bin_io, compare]

module Index = struct
  let key_width = 63
  include Bitvec.Make(struct
      let modulus = Bitvec.modulus key_width
    end)
  include Strings.Index.Persistent.Make(struct
      type t = value
      let compare = compare_value
      let null = {
        id = Int63.zero;
        value = zero
      }
      let succ s = {
        s with
        value = succ s.value
      }
    end)
end

let state = Bap_primus_machine.State.declare
    ~uuid:"873f2ba6-9adc-45bb-8ed1-a0f57337ca80"
    ~name:"value" @@
  Knowledge.return Int63.one

let symbols = Bap_primus_machine.State.declare
    ~uuid:"2d2293e9-4c42-4b82-90a7-7e8e74fd01ed"
    ~name:"symbols" @@
  Knowledge.return Index.empty

let to_word x = x.value
let id x = x.id

let of_word value =
  Machine.Global.get state >>= fun id ->
  Machine.Global.put state (Int63.succ id) >>| fun () ->
  {id;value}

type 'a m = Bitvec.modulus -> 'a Machine.t

external (mod) : 'a m -> Bitvec.modulus -> 'a Machine.t = "%apply"


let inj f x m = of_word Bitvec.(f x mod m)
let proj f {value} = f value
let lift f {value} = of_word (f value)
let lift1 f {value=x} m = of_word Bitvec.(f x mod m)
let lift2 f {value=x} {value=y} m = of_word Bitvec.(f x y mod m)
let of_string x = of_word @@ Bitvec.of_string x
let of_bool x = of_word @@ Bitvec.bool x
let int = inj Bitvec.int
let int32 = inj Bitvec.int32
let int64 = inj Bitvec.int64
let bigint = inj Bitvec.bigint

let zero = of_word Bitvec.zero
let one = of_word Bitvec.one
let ones m = of_word Bitvec.(Bitvec.ones mod m)
let extract ~hi ~lo {value} = of_word Bitvec.(extract ~hi ~lo value)
let append w1 w2 x y = of_word Bitvec.(append w1 w2 x y)
let succ = lift1 Bitvec.succ
let pred = lift1 Bitvec.pred
let nsucc {value=x} n m = of_word Bitvec.(nsucc x n mod m)
let npred {value=x} n m = of_word Bitvec.(npred x n mod m)
let abs = lift1 Bitvec.abs
let neg = lift1 Bitvec.neg

let add = lift2 Bitvec.add
let sub = lift2 Bitvec.sub
let mul = lift2 Bitvec.mul
let div = lift2 Bitvec.div
let sdiv = lift2 Bitvec.sdiv
let rem = lift2 Bitvec.rem
let srem = lift2 Bitvec.srem
let smod = lift2 Bitvec.smod
let nth {value=x} n m = of_word Bitvec.(bool (nth x n mod m))
let lsb {value=x} m = of_word Bitvec.(bool (lsb x mod m))
let msb {value=x} m = of_word Bitvec.(bool (msb x mod m))

let lnot = lift1 Bitvec.lnot
let logand = lift2 Bitvec.logand
let logor = lift2 Bitvec.logor
let logxor = lift2 Bitvec.logxor
let lshift = lift2 Bitvec.lshift
let rshift = lift2 Bitvec.rshift
let arshift = lift2 Bitvec.arshift

module Symbol = struct
  let to_value sym =
    Machine.Local.get symbols >>= fun index ->
    let index = Index.register index sym in
    Machine.Local.put symbols index >>| fun () ->
    Index.key index sym

  let of_value value =
    Machine.Local.get symbols >>| fun index ->
    Index.string index value
end

module Syntax = struct
  let (~-) = neg
  let (+) = add
  let (-) = sub
  let ( * ) = mul
  let (/) = div
  let (/$) = sdiv
  let (%) = rem
  let (%$) = smod
  let (%^) = srem
  let (land) = logand
  let (lor) = logor
  let (lxor) = logxor
  let (lsl) = lshift
  let (lsr) = rshift
  let (asr) = arshift
end

let pp ppf {value} =
  Format.fprintf ppf "%a" Bitvec.pp value


let to_string x =
  Format.asprintf "%a#%a" Bitvec.pp x.value Id.pp x.id
let of_string s = match String.split ~on:'#' s with
  | [w;id] -> {
      value=Bitvec.of_string w;
      id = Int63.of_string id;
    }
  | _ -> failwithf "value: expected <word>#<id> got %s" s ()


include Base.Comparable.Make(struct
    type t = value [@@deriving compare]
    let sexp_of_t x = Sexp.Atom (to_string x)
    let t_of_sexp = function
      | Sexp.List _ -> failwith "value_of_sexp: expected atom"
      | Sexp.Atom s -> of_string s
  end)
