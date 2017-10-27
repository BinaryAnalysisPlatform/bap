open Core_kernel.Std
open Regular.Std
open Bap.Std
open Format
open Bap_primus_types

open Bap_primus_sexp
module Observation = Bap_primus_observation


module Id = struct
  type t = Int63.t
  include Regular.Make(struct
      include Int63
      let version = "1.0.0"
      let module_name = Some "Bap_primus.Std.Value.Id"
    end)
end
type id = Id.t [@@deriving bin_io, compare, sexp]
type t = value [@@deriving bin_io, compare]


let state = Bap_primus_machine.State.declare
    ~uuid:"873f2ba6-9adc-45bb-8ed1-a0f57337ca80"
    ~name:"value"
    (fun proj -> Int63.one)


let to_word x = x.value
let id x = x.id

module Reg = Regular.Make(struct
    type t = value [@@deriving bin_io, compare]
    let pp ppf {id; value} =
      fprintf ppf "%a#%a" Word.pp_hex_full value Id.pp id
    let sexp_of_t x = Sexp.Atom (asprintf "%a" pp x)
    let t_of_sexp = function
      | Sexp.List _ -> failwith "value_of_sexp: expected atom"
      | Sexp.Atom s -> match String.split ~on:'#' s with
        | [w;id] -> {
            value=Word.of_string w;
            id = Int63.of_string id;
          }
        | _ -> failwithf "value: expected <word>#<id> got %s" s ()
    let hash {id} = Int63.hash id
    let pp = pp
    let module_name = Some "Bap_primus.Std.Value"
    let version = "2.0.0"
  end)


module Make(Machine : Machine) = struct
  open Machine.Syntax

  type t = value
  type 'a m = 'a Machine.t

  let id = id
  let of_word value =
    Machine.Global.get state >>= fun id ->
    Machine.Global.put state (Int63.succ id) >>| fun () ->
    {id;value}

  let to_word = to_word
  let inj f x = of_word (f x)
  let proj f {value} = f value
  let lift f {value} = of_word (f value)
  let lift1 f {value=x} = of_word (f x)
  let lift2 f {value=x} {value=y} = of_word (f x y)
  let of_string  = inj Word.of_string
  let of_bool = inj Word.of_bool
  let of_int ~width  = inj (Word.of_int ~width)
  let of_int32 ?width = inj (Word.of_int32 ?width)
  let of_int64 ?width = inj (Word.of_int64 ?width)
  let b0 = of_word Word.b0
  let b1 = of_word Word.b1
  let one = inj Word.one
  let zero = inj Word.zero
  let ones = inj Word.ones
  let signed = lift1 Word.signed
  let is_zero = proj Word.is_zero
  let is_one = proj Word.is_one
  let is_positive = proj Word.is_positive
  let is_negative = proj Word.is_negative
  let is_non_positive = proj Word.is_non_positive
  let is_non_negative = proj Word.is_non_negative

  let bitwidth = proj Word.bitwidth
  let extract ?hi ?lo {value} =
    match Word.extract ?hi ?lo value with
    | Error err -> invalid_arg "Primus.Value.extract: not well typed"
    | Ok x -> of_word x
  let concat = lift2 Word.concat
  let succ = lift1 Word.succ
  let pred = lift1 Word.pred
  let nsucc {value=x} n = of_word (Word.nsucc x n)
  let npred {value=x} n = of_word (Word.npred x n)
  let abs = lift1 Word.abs
  let neg = lift1 Word.neg
  let add = lift2 Word.add
  let sub = lift2 Word.sub
  let mul = lift2 Word.mul
  let div = lift2 Word.div
  let modulo = lift2 Word.modulo
  let lnot {value=x} = of_word (Word.lnot x)
  let logand = lift2 Word.logand
  let logor = lift2 Word.logor
  let logxor = lift2 Word.logxor
  let lshift = lift2 Word.lshift
  let rshift = lift2 Word.rshift
  let arshift = lift2 Word.arshift
  module Syntax = struct
    let (~-) = neg
    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let (/) = div
    let (mod) = modulo
    let (land) = logand
    let (lor) = logor
    let (lxor) = logxor
    let (lsl) = lshift
    let (lsr) = rshift
    let (asr) = arshift
  end


  include Reg
end

include Reg
