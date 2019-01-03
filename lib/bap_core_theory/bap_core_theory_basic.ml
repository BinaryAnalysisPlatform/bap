open Core_kernel
open Bap.Std
open Bap_knowledge

open Bap_core_theory_definition
open Bap_core_theory_sort

open Knowledge.Syntax

let size = Bits.size
let (>>->) x f = x >>= fun x -> f (Value.sort x) x

module Mask = struct

  module Set = struct
    (** [high s n] a bitvector of size [s] with [n] high bits set [1].  *)
    let high width n =
      let n = Word.of_int ~width n in
      Word.(lnot (ones width lsr n))

    (** [low s n] a bitvector of size s with [n] low bits set to [1]  *)
    let low width n =
      let n = Word.of_int ~width n in
      Word.(lnot (ones width lsl n))


    (** [on s hi lo] a mask of size [s] with all bits between [hi] and
        [lo] (both inclusive) set to 1.  *)
    let bits width hi lo =
      let n = (max 0 (hi-lo+1)) in
      Word.(low width n lsl (of_int ~width lo))
  end

  module Unset = struct
    let high m n = Word.lnot (Set.high m n)
    let low m n = Word.lnot (Set.low m n)
    let bits s m n = Word.lnot (Set.bits s m n)
  end


  module Lift(L : Minimal) = struct
    open L
    module Set = struct
      let high s n   = int s @@ Set.high (Bits.size s) n
      let low s n    = int s @@ Set.low (Bits.size s) n
      let bits s m n = int s @@ Set.bits (Bits.size s) m n
    end
    module Unset = struct
      let high s n   = int s @@ Unset.high (Bits.size s) n
      let low s n    = int s @@ Unset.low (Bits.size s) n
      let bits s m n = int s @@ Unset.bits (Bits.size s) m n
    end
  end
end

module Make(L : Minimal) = struct
  open L
  module BitLogic = struct
    let (&&) = and_ and (||) = or_ and not = inv
  end


  include struct
    open BitLogic
    let eq x y =
      x >>= fun x ->
      y >>= fun y ->
      ule !!x !!y && ule !!y !!x
    let neq x y = not (eq x y)
    let slt x y =
      x >>= fun x ->
      y >>= fun y ->
      sle !!x !!y && not (sle !!y !!x)
    let ult x y =
      x >>= fun x ->
      y >>= fun y ->
      ule !!x !!y && not (ule !!y !!x)
    let sgt x y = slt y x
    let ugt x y = ult y x
    let sge x y = sle y x
    let uge x y = ule y x
  end

  let small s x = int s (Word.of_int ~width:(size s) x)
  let zero s = int s (Word.zero (size s))

  let is_zero x =
    x >>-> fun s x ->
    eq !!x (zero s)

  let non_zero x = inv (is_zero x)

  let nsucc x n =
    x >>-> fun s x ->
    add !!x (small s n)

  let npred x n =
    x >>-> fun s x ->
    sub !!x (small s n)

  let succ x = nsucc x 1
  let pred x = npred x 1

  let high s x =
    x >>-> fun t x ->
    let n = min (size t) (max 0 (size t - size s)) in
    cast s b0 (shiftr b0 !!x (small t n))

  let low s x = cast s b0 x
  let signed s x = cast s (msb x) x
  let unsigned s x = cast s b0 x

  module Mask = Mask.Lift(L)

  let bind exp body =
    exp >>-> fun s exp ->
    Var.scoped s @@ fun v ->
    let_ v !!exp (body v)

  let loadw out dir mem key =
    dir >>= fun dir ->
    mem >>-> fun ms mem ->
    key >>= fun key ->
    let vs = Mems.vals ms in
    let chunk_size = size vs in
    let needed = size out in
    let rec loop chunks loaded =
      if loaded < needed then
        let key = nsucc !!key (loaded / chunk_size) in
        bind (load !!mem key) @@ fun chunk ->
        loop (var chunk :: chunks) (loaded + chunk_size)
      else
        ite !!dir
          (concat out (List.rev chunks))
          (concat out chunks) in
    loop [] 0

  let storew dir mem key data =
    data >>-> fun data_t data ->
    mem  >>-> fun mem_t mem ->
    let chunks = Mems.vals mem_t in
    let needed = size data_t and chunk_len = size chunks in
    let nth stored =
      let shift_amount = ite dir
          (small data_t stored)
          (small data_t (needed - stored)) in
      cast chunks b0 (shiftr b0 !!data shift_amount) in
    let rec loop key stored mem =
      if stored < needed then
        loop
          (succ key)
          (stored + chunk_len)
          (store mem key (nth stored))
      else mem in
    loop key 0 !!mem

  let arshift x y = shiftr (msb x) x y
  let rshift x y  = shiftr b0 x y
  let lshift x y  = shiftl b0 x y

  let extract s hi lo x =
    let n = succ (sub hi lo) in
    x >>= fun x ->
    let t = Value.sort x in
    let mask = lshift (not (zero t)) n in
    cast s b0 (logand (not mask) (shiftr b0 !!x lo))
  include L
end
