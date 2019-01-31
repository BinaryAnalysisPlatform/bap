open Core_kernel
open Bap_knowledge

open Bap_core_theory_definition
open Bap_core_theory_sort

open Knowledge.Syntax

let size = Bits.size
let (>>->) x f = x >>= fun x -> f (Value.sort x) x


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

  let small s x = int s Bitvec.(int x mod modulus (size s))
  let zero s = int s Bitvec.zero

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
