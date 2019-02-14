open Core_kernel
open Bap.Std

open Bap_knowledge
open Bap_core_theory
open Knowledge.Syntax

module Elementary (Theory : Theory.Core) = struct
  open Theory

  type 'a t = 'a knowledge

  exception Not_a_table

  let bits fsort = Floats.(Format.bits (format fsort))

  let name op sort rank =
    String.concat ~sep:"/" [op; Sort.name sort; string_of_int rank]

  let scheme ident =
    match String.split ~on:'/' (Var.Ident.to_string ident) with
    | [name; sort; rank] -> Some (name, sort, rank)
    | _ -> None

  let operation ident = match scheme ident with
    | Some (name,_,_) -> name
    | None -> raise Not_a_table

  let table = name
  let is_table ident = Option.is_some (scheme ident)

  let bind a body =
    a >>= fun a ->
    let sort = Value.sort a in
    Var.scoped sort @@ fun v ->
    let_ v !!a (body v)

  let (>>>=) = bind

  let (>>->) x f =
    x >>= fun x ->
    f (Value.sort x) x

  let approximate ~rank ~reduce ~extract ~coefs x rmode =
    x >>-> fun fsort x ->
    reduce !!x >>>= fun key ->
    load (var coefs) (var key) >>>= fun value ->
    let coef i = float fsort (extract i (var value)) in
    let rec sum i y =
      if i >= 0 then
        fmul rmode !!x y >>>= fun y ->
        coef i >>>= fun c ->
        fadd rmode (var y) (var c) >>>= fun y ->
        sum (i - 1) (var y)
      else y in
    coef rank >>>= fun cr ->
    if rank = 0 then var cr
    else sum (rank - 1) (var cr)

  let of_int sort x = int sort (Word.of_int ~width:(Bits.size sort) x)

  let nth fsort n bitv =
    bitv >>-> fun sort bitv ->
    let index = of_int sort (n * Bits.size (bits fsort)) in
    lshift !!bitv index >>>= fun bitv ->
    high (bits fsort) (var bitv)

  let tabulate op ~rank ~size x rmode =
    x >>-> fun fsort x ->
    let keys = Bits.define size in
    let values = Bits.define ((rank + 1) * Bits.size (bits fsort)) in
    let mems = Mems.define keys values in
    let name = name op fsort rank in
    let coefs = Var.define mems name in
    let reduce x = high keys (fbits x) in
    let extract = nth fsort in
    approximate ~rank ~coefs ~reduce ~extract !!x rmode

  module Scheme = struct
    type 'a t = 'a sort -> int -> string

    let pow s = name "pow" s
    let powr s = name "powr" s
    let compound  s = name "compound" s
    let rootn s = name "rootn" s
    let pownn s = name "pownn" s
    let rsqrt s = name "rsqrt" s
    let hypot s = name "hypot" s
    let exp  s = name "exp" s
    let expm1 s = name "expm1" s
    let exp2 s = name "exp2" s
    let exp2m1 s = name "exp2m1" s
    let exp10 s = name "exp10" s
    let exp10m1 s = name "exp10m1" s
    let log s = name "log" s
    let log2 s = name "log2" s
    let log10 s = name "log10" s
    let logp1 s = name "logp1" s
    let log2p1 s = name "log2p1" s
    let log10p1 s = name "log10p1" s
    let sin s = name "sin" s
    let cos s = name "cos" s
    let tan s = name "tan" s
    let sinpi s = name "sinpi" s
    let cospi s = name "cospi" s
    let atanpi s = name "atanpi" s
    let atan2pi s = name "atan2pi" s
    let asin s = name "asin" s
    let acos s = name "acos" s
    let atan s = name "atan" s
    let atan2 s = name "atan2" s
    let sinh s = name "sinh" s
    let cosh s = name "cosh" s
    let tanh s = name "tanh" s
    let asinh s = name "asinh" s
    let acosh s = name "acosh" s
    let atanh s = name "atanh" s
  end

end
