open Core_kernel
open Bap.Std

open Bap_knowledge
open Bap_core_theory
open Knowledge.Syntax

module Elementary = struct

  type 'a t = 'a knowledge

  exception Not_a_table

  let bits fsort = Floats.(Format.bits (format fsort))

  let name op sort rank =
    String.concat ~sep:"/" [op; Sort.name sort; string_of_int rank]

  let scheme tab =
    match String.split ~on:'/' (Var.name tab) with
    | [name; sort; rank] -> Some (name, sort, rank)
    | _ -> None

  let operation tab = match scheme tab with
    | Some (name,_,_) -> name
    | None -> raise Not_a_table

  let table = name
  let is_table tab = Option.is_some (scheme tab)

  module Make(Theory : Theory.Core) = struct
    open Theory

    let bind a body =
      a >>= fun a ->
      let sort = Value.sort a in
      Var.scoped sort @@ fun v ->
      let_ v !!a (body v)

    let (>>>=) = bind

    let (>>->) x f =
      x >>= fun x ->
      f (Value.sort x) x

    let approximate ~rank ~reduce ~extract ~coefs ?(rmode=rne) x =
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

    let tabulate op ~rank ~interval x =
      x >>-> fun fsort x ->
      let keys = Bits.define interval in
      let values = Bits.define ((rank + 1) * Bits.size (bits fsort)) in
      let mems = Mems.define keys values in
      let name = name op fsort rank in
      let coefs = Var.define mems name in
      let reduce x = high keys (fbits x) in
      let extract = nth fsort in
      approximate ~rank ~coefs ~reduce ~extract !!x

  end

  module Base = Make(Theory.Manager)

  let approximate = Base.approximate
  let tabulate = Base.tabulate

  module Scheme = struct
    let pow = name "pow"
    let powr = name "powr"
    let compound = name "compound"
    let rootn = name "rootn"
    let pownn = name "pownn"
    let rsqrt = name "rsqrt"
    let hypot = name "hypot"
    let exp  = name "exp"
    let expm1 = name "expm1"
    let exp2 = name "exp2"
    let exp2m1 = name "exp2m1"
    let exp10 = name "exp10"
    let exp10m1 = name "exp10m1"
    let log = name "log"
    let log2 = name "log2"
    let log10 = name "log10"
    let logp1 = name "logp1"
    let log2p1 = name "log2p1"
    let log10p1 = name "log10p1"
    let sin = name "sin"
    let cos = name "cos"
    let tan = name "tan"
    let sinpi = name "sinpi"
    let cospi = name "cospi"
    let atanpi = name "atanpi"
    let atan2pi = name "atan2pi"
    let asin = name "asin"
    let acos = name "acos"
    let atan = name "atan"
    let atan2 = name "atan2"
    let sinh = name "sinh"
    let cosh = name "cosh"
    let tanh = name "tanh"
    let asinh = name "asinh"
    let acosh = name "acosh"
    let atanh = name "atanh"
  end
end
