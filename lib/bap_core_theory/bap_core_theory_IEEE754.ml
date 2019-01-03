open Core_kernel
open Bap_core_theory_sort


type ('b,'e,'t) ieee754 = IEEE754 : ('b,'e,'s) ieee754
type ('b,'e,'t) t = ('b,'e,'t) ieee754

(* see IEEE754 3.6 *)
type parameters = {
  base : int;
  bias : int;
  k : int;
  p : int;
  w : int;
  t : int;
}

let (^) b e = Int.of_float (float b ** float e)

let log2 n = log n /. log 2.
let round x = round ~dir:`Nearest x

let prec k =
  let k = float k in Int.of_float @@
  k -. round(4. *. log2 k) +. 13.

let ebits k =
  let k = float k in Int.of_float @@
  round(4. *. log2 k) -. 13.

let bias k p = (2^(k-p-1))-1

let binary k =
  let p = prec k and w = ebits k in {
    base = 2;
    k; w; p;
    t = k - w - 1;
    bias = bias k p;
  }

let decimal k =
  let p = 9 * k/32 - 2 in
  let exp = k / 16 + 3 in
  let emax = 3 * Int.of_float (2. ** float exp) in {
    base = 10;
    bias = emax + p - 2;
    w = k / 16 + 9;
    t = 15 * k / 16 - 10;
    k; p;
  }

let binary16 = {
  base = 2;
  bias = 15;
  k = 16;
  p = 11;
  w = 5;
  t = 10;
}

let binary32 = {
  base = 2;
  bias = 127;
  k = 32;
  p = 24;
  w = 8;
  t = 23;
}

let binary80 = {
  base = 2;
  bias = 16383;
  k = 80;
  p = 64;
  w = 15;
  t = 64;
}

let binary64 = binary 64
let binary128 = binary 128
let decimal32 = decimal 32
let decimal64 = decimal 64
let decimal128 = decimal 128

module Sort = struct
  let format {base; k} =
    let base = if base = 2 then "Binary" else "Decimal" in
    let exp = Sort.(Cons ("IEEE754", [Sort (Cons (base,[]))])) in
    Floats.Format.define exp IEEE754 (Bits.define k)

  let define p = Floats.define (format p)

  let spec e =
    let fmt = Floats.format e in
    let k = Bits.size (Floats.Format.bits fmt) in
    match Floats.Format.exp fmt with
    | Sort.Cons ("IEEE754", [Sort (Cons ("Binary", []))])  -> binary k
    | Sort.Cons ("IEEE754", [Sort (Cons ("Decimal", []))]) -> decimal k
    | _ -> assert false

  let exps e =
    let {w} = spec e in
    Bits.define w

  let sigs e =
    let {p} = spec e in
    Bits.define p

  let bits e =
    let {k} = spec e in
    Bits.define k
end

let binary = function
  | 0  -> None
  | 16 -> Some binary16
  | 32 -> Some binary32
  | n when n mod 32 = 0 -> Some (binary n)
  | _ -> None

let decimal n =
  if n > 0 && n mod 32 = 0 then Some (decimal n)
  else None
