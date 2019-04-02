open Core_kernel
open Bap.Std

open Bap_knowledge
open Bap_core_theory
open Knowledge.Syntax


type 'a t = 'a knowledge

type ('b,'e,'t,'s) fsort = (('b,'e,'t) Theory.IEEE754.t,'s) Theory.format Theory.Float.t

module Value = Knowledge.Value

module Make(B : Theory.Core) = struct

  open Knowledge.Syntax

  module B = struct
    include B

    let one s = succ (zero s)
    let ones s = not (zero s)

    let is_one x =
      x >>= fun v -> eq x (one (Value.cls v))

    let is_all_ones x =
      x >>= fun v -> eq x (ones (Value.cls v))

    let of_int sort v =
      let m = Bitvec.modulus (Theory.Bitv.size sort) in
      int sort Bitvec.(int v mod m)

    let is_negative = B.msb
    let is_positive x = B.inv (B.msb x)
    let is_non_negative x = B.or_ (is_positive x) (B.is_zero x)
    let abs x = ite (is_negative x) (neg x) x

    let of_bool = function
      | true -> b1
      | false -> b0

    let testbit x i = lsb (rshift x i)

    let if_ cond ~then_ ~else_ = ite cond then_ else_

    module Infix = struct
      let ( + ) = add
      let ( - ) = sub
      let ( * ) = mul
      let ( / ) = div
      let ( = ) = eq
      let ( <> ) = neq
      let ( < ) = ult
      let ( > ) = ugt
      let ( <= ) = ule
      let ( >= ) = uge
      let ( <$ ) = slt
      let ( >$ ) = sgt
      let ( <=$ ) = sle
      let ( >=$ ) = sge
      let ( lsr ) = rshift
      let ( lsl ) = lshift
      let ( land ) = logand
      let ( lor ) = logor
      let ( lxor ) = logxor
      let (&&) = and_
      let (||) = or_
    end

    include Infix

    let max x y = ite (x > y) x y
    let min x y = ite (x < y) x y
    let smax x y = ite (x >$ y) x y
    let smin x y = ite (x <$ y) x y

    let leading_one sort =
      one sort lsl (of_int sort Caml.(Theory.Bitv.size sort - 1))

  end

  type 'a t = 'a knowledge

  module Bits = Theory.Bitv
  module IEEE754 = Theory.IEEE754

  let (>>->) x f =
    x >>= fun x ->
    f (Value.cls x) x

  let bind a body =
    a >>= fun a ->
    let sort = Value.cls a in
    Theory.Var.scoped sort @@ fun v ->
    B.let_ v !!a (body (B.var v))

  let (>>>=) = bind

  let exps = Theory.IEEE754.Sort.exps

  let sigs fsort =
    let open Theory.IEEE754 in
    let spec = Sort.spec fsort in
    Theory.Bitv.define spec.p

  let floats fsort = exps fsort, sigs fsort

  let bits = IEEE754.Sort.bits

  let fsign = B.msb

  let exponent fsort bitv =
    let open IEEE754 in
    let bits = Sort.bits fsort in
    let exps = Sort.exps fsort in
    let spec = Sort.spec fsort in
    B.(low exps (bitv lsr of_int bits spec.t))

  (* pre: input coef is already of t-bits length  *)
  let pack_raw fsort sign expn coef =
    let open B in
    let open IEEE754 in
    let bits = Sort.bits fsort in
    let bit  = Bits.define 1 in
    let bits_1 = Bits.define Caml.(Bits.size bits - 1) in
    let sign = ite sign (B.one bit) (B.zero bit) in
    B.append bits sign (B.append bits_1 expn coef)

  let pack fsort sign expn coef =
    let open B in
    let {IEEE754.p; t} = IEEE754.Sort.spec fsort in
    let is_subnormal = (inv (msb coef)) && (is_one expn) in
    ite is_subnormal (pred expn) expn >>>= fun expn ->
    if Caml.(p = t) then pack_raw fsort sign expn coef
    else
      B.low (Bits.define t) coef >>>= fun coef ->
      pack_raw fsort sign expn coef >>= fun r -> !!r

  let raw_significand fsort bitv =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    let sigs = Bits.define spec.t in
    B.low sigs bitv

  let finite_significand fsort expn bitv =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    raw_significand fsort bitv >>>= fun coef ->
    if spec.t = spec.p then coef
    else
      let bit = Bits.define 1 in
      let leading_bit = B.(ite (is_zero expn) (zero bit) (one bit)) in
      B.append (sigs fsort) leading_bit coef

  let unpack fsort x f =
    x >>>= fun x ->
    exponent fsort x >>>= fun expn ->
    fsign x >>>= fun sign ->
    finite_significand fsort expn x >>>= fun coef ->
    B.(ite (is_zero expn) (succ expn) expn) >>>= fun expn ->
    f sign expn coef

  let unpack_raw fsort x f =
    x >>>= fun x ->
    exponent fsort x >>>= fun expn ->
    raw_significand fsort x >>>= fun coef ->
    f (fsign x) expn coef

  let with_sign sign bitv =
    let open B in
    bitv >>-> fun s bitv ->
    let s' = Bits.define Caml.(Bits.size s - 1) in
    let bit = Bits.define 1 in
    ite sign ((append s (one bit) (zero s')) lor !!bitv)
      ((append s (zero bit) (ones s')) land !!bitv)

  let fzero fsort sign =
    let open B in
    let bits = IEEE754.Sort.bits fsort in
    zero bits >>>= fun bitv ->
    ite sign (
      ones bits >>>= fun ones ->
      not (ones lsr one bits) >>>= fun one ->
      one lor bitv) bitv

  let fone fsort sign =
    let open IEEE754 in
    let {bias; t} = Sort.spec fsort in
    let expn = B.of_int (Sort.exps fsort) bias in
    let sigs = Bits.define t in
    pack_raw fsort sign expn (B.zero sigs)

  let inf fsort sign =
    let open B in
    let exps = IEEE754.Sort.exps fsort in
    pack fsort sign (B.ones exps) (B.zero (sigs fsort))

  let is_inf fsort x : Theory.bool =
    unpack fsort x @@ fun _ expn coef ->
    B.(and_ (is_zero coef) (is_all_ones expn))

  let is_pinf fsort x =
    is_inf fsort x >>>= fun inf ->
    B.(and_ inf (inv (msb x)))

  let is_ninf fsort x =
    is_inf fsort x >>>= fun inf ->
    B.(and_ inf (msb x))

  let is_qnan fsort x =
    let open B in
    unpack_raw fsort x @@ fun _sign expn coef ->
    is_all_ones expn && non_zero coef && msb coef

  let is_snan fsort x =
    let open B in
    unpack_raw fsort x @@ fun _sign expn coef ->
    is_all_ones expn && non_zero coef && inv (msb coef)

  let is_nan fsort x =
    let open B in
    unpack_raw fsort x @@ fun _sign expn coef ->
    is_all_ones expn && non_zero coef

  let qnan fsort =
    let open B in
    let open IEEE754 in
    let exps = Sort.exps fsort in
    let spec = Sort.spec fsort in
    let sigs = Bits.define spec.t in
    not (ones sigs lsr one sigs) >>>= fun coef ->
    pack_raw fsort B.b1 (ones exps) coef

  let snan fsort =
    let open B in
    let open IEEE754 in
    let exps = Sort.exps fsort in
    let spec = Sort.spec fsort in
    let sigs = Bits.define spec.t in
    pack_raw fsort B.b0 (ones exps) (B.one sigs)

  (* unset a leading bit in coef, no-checks for nan are performed *)
  let transform_to_signal fsort x =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    let bits = Sort.bits fsort in
    let shift = B.of_int bits (spec.t - 1) in
    let mask = B.(not (one bits lsl shift)) in
    B.(x land mask)

  (* set a leading bit in coef, no-checks for nan are performed *)
  let transform_to_quite fsort x =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    let bits = Sort.bits fsort in
    let shift = B.of_int bits (spec.t - 1) in
    let mask = B.(one bits lsl shift) in
    B.(x lor mask)

  let with_special fsort x f =
    let open B in
    unpack_raw fsort x @@ fun _sign expn coef ->
    is_all_ones expn >>>= fun is_special ->
    (is_special && is_zero coef) >>>= fun is_inf ->
    (is_special && non_zero coef && inv (msb coef)) >>>= fun is_snan ->
    (is_special && msb coef) >>>= fun is_qnan ->
    f ~is_inf ~is_snan ~is_qnan

  let is_special fsort x = unpack fsort x @@ fun _ expn _ -> B.is_all_ones expn
  let is_finite fsort x = B.inv (is_special fsort x)
  let is_finite_nonzero fsort x =
    let open B in
    unpack_raw fsort x @@ fun _ expn coef ->
    inv (is_all_ones expn) >>>= fun ok_expn ->
    ok_expn && (non_zero expn || non_zero coef)

  let is_norml fsort x =
    unpack_raw fsort x @@ fun _ e _ ->
    B.(non_zero e && inv (is_all_ones e))
  let is_subnormal fsort x =
    unpack_raw fsort x @@ fun _ e _ -> B.is_zero e

  let is_zero x =
    let open B in
    x >>-> fun s x ->
    is_zero ((!!x lsl one s) lsr one s)

  (* TODO: just a stub, need more reliable functions  *)
  let fsucc fsort x =
    let open B in
    let exps,sigs = floats fsort in
    unpack fsort x @@ fun sign expn coef ->
    succ coef >>>= fun coef ->
    ite (is_zero coef) (one exps) (zero exps) >>>= fun de ->
    ite (is_zero coef) (leading_one sigs) coef >>>= fun coef ->
    expn + de >>>= fun expn ->
    pack fsort sign expn coef

  (* TODO: just a stub, need more reliable functions  *)
  let fpred fsort x =
    let open B in
    let exps,_sigs = floats fsort in
    unpack fsort x @@ fun sign expn coef ->
    pred coef >>>= fun coef ->
    ite (is_all_ones coef) (one exps) (zero exps) >>>= fun de ->
    expn - de >>>= fun expn ->
    pack fsort sign expn coef

  let precision fsort = let {IEEE754.p} = IEEE754.Sort.spec fsort in p
  let bias fsort = let {IEEE754.bias} = IEEE754.Sort.spec fsort in bias

  let match_ ?default cases =
    let cases, default = match default with
      | Some d -> List.rev cases, d
      | None ->
        let cases = List.rev cases in
        let _,d = List.hd_exn cases in
        List.tl_exn cases, d in
    List.fold cases ~init:default
      ~f:(fun fin (cond, ok) -> B.ite cond ok fin)

  let (-->) x y = x,y
  let anything_else = B.b1


  let extract_last x n =
    let open B in
    x >>-> fun xsort x ->
    n >>-> fun nsort n ->
    let mask = ones xsort lsl !!n in
    let size = Bits.size xsort |> B.of_int nsort in
    ite (!!n = size) !!x (!!x land (not mask))

  let is_round_up rm sign last guard round sticky =
    let open B in
    let case m t f = ite (requal rm m) t f and default = ident in
    case rtn (inv sign) @@
    case rtz sign @@
    case rna guard @@
    case rne (guard && (round || sticky || last)) @@
    default b0

  let guardbits loss lost_bits f =
    let open B in
    lost_bits >>-> fun sort lost_bits ->
    (!!lost_bits > zero sort) >>>= fun has_grd ->
    (!!lost_bits > one sort ) >>>= fun has_rnd ->
    (!!lost_bits > of_int sort 2) >>>= fun has_stk ->
    ite has_grd (pred !!lost_bits) (zero sort) >>>= fun grd_pos ->
    ite has_rnd (pred grd_pos) (zero sort) >>>= fun rnd_pos ->
    ite has_grd (testbit loss grd_pos) b0 >>>= fun grd ->
    ite has_rnd (testbit loss rnd_pos) b0 >>>= fun rnd ->
    ite has_stk (non_zero (extract_last loss rnd_pos)) b0 >>>= fun stk ->
    f grd rnd stk

  let round rm sign coef loss lost_bits f =
    let open B in
    guardbits loss lost_bits @@ fun grd rnd stk ->
    ite (is_round_up rm sign (lsb coef) grd rnd stk) (succ coef) coef >>>= fun coef' ->
    and_ (non_zero coef) (is_zero coef') >>>= fun is_overflow ->
    f coef' is_overflow

  (* maximum possible exponent that fits in [n - 1] bits. (one for sign)
     and one for special numbers like inf or nan *)
  let max_exponent' n = int_of_float (2.0 ** (float_of_int n )) - 2
  let min_exponent' _n = 1
  let max_exponent  n = B.of_int n (Bits.size n |> max_exponent')
  let min_exponent  n = B.of_int n (Bits.size n |> min_exponent')

  (* returns pow of 2 nearest to n and 2 in that power *)
  let nearest_pow2 num =
    let rec find pow n =
      let n' = 2 * n in
      if n' >= num then pow + 1, n'
      else find (pow + 1) n' in
    find 0 1

  let clz x =
    let open B in
    x >>-> fun sort x ->
    let size = Bits.size sort in
    let pow, num = nearest_pow2 size in
    let sort' = Bits.define num in
    let shifts = List.init pow ~f:(fun p -> Caml.(num / (2 lsl p))) in
    let shifts,_ =
      List.fold shifts ~init:([],0)
        ~f:(fun (acc,prev) curr ->
            let total = Caml.(curr + prev) in
            (total, curr) :: acc, total) in
    let shifts = List.rev shifts in
    let rec loop lets x = function
      | [] -> List.fold lets ~f:(+) ~init:(zero sort)
      | (total, shf) :: shifts ->
        ones sort' lsl (of_int sort total) >>>= fun mask ->
        ite (is_zero (x land mask)) (x lsl of_int sort shf) x >>>= fun nextx ->
        ite (is_zero (x land mask)) (of_int sort shf) (zero sort) >>>= fun nextn ->
        loop (nextn :: lets) nextx shifts in
    loop [] (B.unsigned sort' !!x) shifts >>>= fun n ->
    of_int sort Caml.(num - size) >>>= fun dif ->
    ite (is_zero !!x) (of_int sort size) (n - dif)

  let possible_lshift expn coef =
    let open B in
    expn >>-> fun exps expn ->
    coef >>-> fun sigs coef ->
    clz !!coef >>>= fun clz ->
    min_exponent exps >>>= fun mine ->
    ite (!!expn < mine) (zero exps) (!!expn - mine) >>>= fun diff ->
    unsigned sigs diff >>>= fun diff ->
    ite (clz < diff) clz diff

  let norm_finite expn coef f =
    let open B in
    expn >>-> fun exps expn ->
    coef >>-> fun _sigs coef ->
    possible_lshift !!expn !!coef >>>= fun shift ->
    unsigned exps shift >>>= fun dexpn ->
    !!coef lsl shift >>>= fun coef ->
    ite (is_zero coef) (min_exponent exps) (!!expn - dexpn) >>>= fun expn ->
    f expn coef

  let norm expn coef f =
    let open B in
    expn >>-> fun _exps expn ->
    ite (is_all_ones !!expn) (f !!expn coef)
      (norm_finite !!expn coef f)

  let msbn x =
    let open B in
    x >>-> fun sort x ->
    clz !!x >>>= fun clz ->
    of_int sort (Bits.size sort) - clz - one sort

  let xor s s' = B.(and_ (or_ s s') (inv (and_ s s')))

  let guardbits' overflow last loss lost_bits f =
    let open B in
    guardbits loss lost_bits @@ fun guard' round' sticky' ->
    ite overflow last guard' >>>= fun guard ->
    ite overflow guard' round' >>>= fun round ->
    ite overflow (round' || sticky') sticky' >>>= fun sticky ->
    f guard round sticky

  let fadd_finite fsort rm x y =
    let open B in
    let exps, sigs = floats fsort in
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun _ yexpn ycoef ->
    ite (xexpn > yexpn) (xexpn - yexpn) (yexpn - xexpn) >>>= fun lost_bits ->
    match_ [
      (xexpn = yexpn) --> zero sigs;
      (xexpn > yexpn) --> extract_last ycoef lost_bits;
      (xexpn < yexpn) --> extract_last xcoef lost_bits;
    ] >>>= fun loss ->
    ite (xexpn > yexpn) xcoef (xcoef lsr lost_bits) >>>= fun xcoef ->
    ite (yexpn > xexpn) ycoef (ycoef lsr lost_bits) >>>= fun ycoef ->
    xcoef + ycoef >>>= fun sum ->
    max xexpn yexpn >>>= fun expn ->
    ite (sum >= xcoef) expn (succ expn) >>>= fun expn ->
    guardbits' (sum < xcoef) (lsb sum) loss lost_bits @@ fun guard round sticky ->
    ite (sum < xcoef) (sum lsr one sigs) sum >>>= fun coef ->
    ite (sum < xcoef) (coef lor leading_one sigs) coef >>>= fun coef ->
    is_round_up rm xsign (lsb coef) guard round sticky >>>= fun up ->
    ite up (succ coef) coef >>>= fun coef' ->
    (is_zero coef' && non_zero coef) >>>= fun rnd_overflow ->
    ite rnd_overflow (leading_one sigs) coef' >>>= fun coef ->
    ite rnd_overflow (succ expn) expn >>>= fun _expn' ->
    ite (expn > max_exponent exps) (zero sigs) coef >>>= fun coef ->
    norm expn coef @@ fun expn coef ->
    pack fsort xsign expn coef

  let bitv_extend bitv ~addend f =
    bitv >>-> fun sort bitv ->
    let sort' = Bits.define (Bits.size sort + addend) in
    B.unsigned sort' !!bitv >>>= fun bitv ->
    f bitv sort'

  let common_ground xexpn xcoef yexpn ycoef f =
    let open B in
    xexpn >>-> fun exps xexpn ->
    xcoef >>-> fun sigs xcoef ->
    let xexpn = !!xexpn in
    let xcoef = !!xcoef in
    ite (xexpn > yexpn) (xexpn - yexpn) (yexpn - xexpn) >>>= fun diff ->
    ite (is_zero diff) diff (diff - one exps) >>>= fun lost_bits ->
    match_ [
      (xexpn > yexpn) --> (
        extract_last ycoef lost_bits >>>= fun loss ->
        xcoef lsl one sigs >>>= fun xcoef ->
        ycoef lsr lost_bits >>>= fun ycoef ->
        f loss lost_bits xcoef ycoef);
      (xexpn < yexpn) --> (
        extract_last xcoef lost_bits >>>= fun loss ->
        xcoef lsr lost_bits >>>= fun xcoef ->
        ycoef lsl one sigs >>>= fun ycoef ->
        f loss lost_bits xcoef ycoef);
      (xexpn = yexpn) --> f (zero sigs) (zero exps) xcoef ycoef;
    ]

  let fsub_finite fsort rm x y =
    let open B in
    let exps, sigs = floats fsort in
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun _ysign yexpn ycoef ->
    bitv_extend xcoef ~addend:1 @@ fun xcoef sigs' ->
    bitv_extend ycoef ~addend:1 @@ fun ycoef _ ->
    or_ (xexpn < yexpn) (and_ (xexpn = yexpn) (xcoef < ycoef)) >>>= fun swap ->
    ite swap (inv xsign) xsign >>>= fun sign ->
    ite (xexpn = yexpn && xcoef = ycoef) b0 sign >>>= fun sign ->
    common_ground xexpn xcoef yexpn ycoef @@ fun loss lost_bits xcoef ycoef ->
    ite (is_zero loss) (zero sigs') (one sigs') >>>= fun borrow ->
    ite swap (ycoef - xcoef - borrow) (xcoef - ycoef - borrow) >>>= fun coef ->
    msb coef >>>= fun msbc ->
    max xexpn yexpn >>>= fun expn ->
    ite (xexpn = yexpn) expn (expn - one exps) >>>= fun expn ->
    ite (is_zero coef) (min_exponent exps) (ite msbc (succ expn) expn) >>>= fun expn ->
    guardbits loss lost_bits @@ fun guard' round' sticky' ->
    ite (round' || sticky') (inv guard') guard' >>>= fun guard' ->
    ite msbc (lsb coef) guard' >>>= fun guard ->
    ite msbc guard' round' >>>= fun round ->
    ite msbc (round' || sticky') sticky' >>>= fun sticky ->
    ite msbc (coef lsr one sigs') coef >>>= fun coef ->
    is_round_up rm sign (lsb coef) guard round sticky >>>= fun up ->
    unsigned sigs coef >>>= fun coef ->
    ite up (succ coef) coef >>>= fun coef' ->
    (is_zero coef' && non_zero coef) >>>= fun rnd_overflow ->
    ite rnd_overflow (leading_one sigs) coef' >>>= fun coef ->
    ite rnd_overflow (succ expn) expn >>>= fun expn ->
    norm expn coef @@ fun expn coef -> pack fsort sign expn coef

  let add_or_sub_finite is_sub fsort rm x y =
    let ( lxor ) = xor in
    let s1 = is_sub in
    let s2 = fsign x in
    let s3 = fsign y in
    let is_sub = s1 lxor (s2 lxor s3) in
    B.ite is_sub (fsub_finite fsort rm x y)
      (fadd_finite fsort rm x y)

  let fsum_special fsort is_sub x y =
    let open B in
    let not = inv in
    let ( lxor ) = xor in
    let s1 = is_sub in
    let s2 = fsign x in
    let s3 = fsign y in
    let is_sub = s1 lxor (s2 lxor s3) in
    with_special fsort x @@ fun ~is_inf:xinf ~is_snan:xsnan ~is_qnan:xqnan ->
    with_special fsort y @@ fun ~is_inf:yinf ~is_snan:ysnan ~is_qnan:yqnan ->
    (xsnan || xqnan) >>>= fun xnan ->
    (ysnan || yqnan) >>>= fun ynan ->
    (xinf && yinf) >>>= fun is_inf ->
    match_ [
      (is_sub && is_inf) --> qnan fsort;
      (xinf && yinf) --> x;
      (xnan && (not ynan)) --> transform_to_quite fsort x;
      (ynan && (not xnan)) --> transform_to_quite fsort y;
      anything_else --> (transform_to_quite fsort x);
    ]

  let add_or_sub ~is_sub fsort rm x y =
    is_finite fsort x >>>= fun is_x_fin ->
    is_finite fsort y >>>= fun is_y_fin ->
    let open B in
    ite (is_x_fin && is_y_fin)
      (add_or_sub_finite is_sub fsort rm x y)
      (fsum_special fsort is_sub x y)

  let fsub fsort rm x y = add_or_sub ~is_sub:B.b1 fsort rm x y
  let fadd fsort rm x y = add_or_sub ~is_sub:B.b0 fsort rm x y

  let double bitv f =
    bitv >>-> fun sort bitv ->
    bitv_extend !!bitv ~addend:(Bits.size sort) f

  let normalize_coef coef f =
    let open B in
    coef >>-> fun _sort coef ->
    clz !!coef >>>= fun clz ->
    !!coef lsl clz >>>= fun coef ->
    f coef clz

  (* Clarification.
     The result of (Sx,Ex,Cx) * (Sy,Ey,Cy) is (Sx xor Sy, Ex + Ey - bias, Cx * Cy),
     where S,E,C - sign, exponent and coefficent.
     Also, say we have 53-bit precision: C = c52 . c51 . c50 ... c0.
     We normalize operands by shifting them as left as possible to
     be sure what exactly bits the result will occupy.
     The bare result is C = c105 . c104 ... c0. If c105 is set then
     we have an overflow and right shift needed. The result of multiplication
     is in 53 bits starting from c104.*)
  let fmul_finite fsort rm x y =
    let open B in
    let double e c f =
      double e @@ fun e es ->
      double c @@ fun c cs ->
      f e es c cs in
    let precision = precision fsort in
    let exps,sigs = floats fsort in
    let mine = min_exponent' (Bits.size exps) in
    let maxe = max_exponent' (Bits.size exps) in
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun ysign yexpn ycoef ->
    xor xsign ysign >>>= fun sign ->
    normalize_coef xcoef @@ fun xcoef dx ->
    normalize_coef ycoef @@ fun ycoef dy ->
    dx + dy >>>= fun dnorm ->
    double xexpn xcoef @@ fun xexpn exps' xcoef sigs' ->
    double yexpn ycoef @@ fun yexpn _ ycoef _ ->
    of_int sigs' precision >>>= fun prec ->
    xexpn + yexpn >>>= fun expn ->
    xcoef * ycoef >>>= fun coef ->
    msb coef >>>= fun coef_overflowed ->
    ite coef_overflowed (succ expn) expn >>>= fun expn ->
    of_int exps' (bias fsort) >>>= fun bias ->
    bias + unsigned exps' dnorm >>>= fun dexpn ->
    ite (dexpn >=$ expn) (dexpn - expn + of_int exps' mine) (zero exps') >>>= fun underflow ->
    underflow > of_int exps' precision >>>= fun is_underflow ->
    expn - dexpn + underflow >>>= fun expn ->
    expn > of_int exps' maxe >>>= fun is_overflow ->
    ite coef_overflowed (one sigs') (zero sigs') >>>= fun from_overflow ->
    coef lsr (from_overflow + unsigned sigs' underflow) >>>= fun coef ->
    coef lsl one sigs' >>>= fun coef ->
    low sigs coef  >>>= fun loss ->
    high sigs coef >>>= fun coef ->
    low exps expn  >>>= fun expn ->
    round rm sign coef loss prec @@ fun coef rnd_overflow ->
    ite rnd_overflow (leading_one sigs) coef >>>= fun coef ->
    ite rnd_overflow (succ expn) expn >>>= fun expn ->
    ((expn > of_int exps maxe) || is_overflow) >>>= fun is_overflow ->
    norm expn coef @@ fun expn coef ->
    ite is_underflow (zero exps) expn >>>= fun expn ->
    ite is_overflow (ones exps) expn >>>= fun expn ->
    ite (is_overflow || is_underflow) (zero sigs) coef >>>= fun coef ->
    pack fsort sign expn coef

  let fmul_special fsort x y =
    let open B in
    with_special fsort x @@ fun ~is_inf:xinf ~is_snan:xsnan ~is_qnan:xqnan ->
    with_special fsort y @@ fun ~is_inf:yinf ~is_snan:_ysnan ~is_qnan:_yqnan ->
    fsign x >>>= fun xsign ->
    fsign y >>>= fun ysign ->
    (xinf && yinf) >>>= fun is_inf ->
    match_ [
      (is_zero x && yinf) --> qnan fsort;
      (is_zero y && xinf) --> qnan fsort;
      is_inf --> with_sign (xor xsign ysign) x;
      (xsnan || xqnan) --> transform_to_quite fsort x;
      anything_else --> (transform_to_quite fsort y);
    ]

  let fmul fsort rm x y =
    is_finite fsort x >>>= fun is_x_fin ->
    is_finite fsort y >>>= fun is_y_fin ->
    B.(ite (is_x_fin && is_y_fin)
         (fmul_finite fsort rm x y)
         (fmul_special fsort x y))

  let mask_bit sort i =
    let uno = B.one sort in
    let shf = B.of_int sort i in
    B.(uno lsl shf)

  (* pre: nominator > denominator  *)
  let long_division prec nomin denom f =
    let open B in
    let lost_alot = b1, b1, b0 in
    let lost_half = b1, b0, b0 in
    let lost_zero = b0, b0, b0 in
    let lost_afew = b0, b0, b1 in
    nomin >>-> fun sort nomin ->
    let rec loop i bits nomin =
      if Caml.(i < 0) then
        List.fold bits ~f:(lor) ~init:(zero sort) >>>= fun coef ->
        match_ [
          (nomin > denom) --> f coef lost_alot;
          (nomin = denom) --> f coef lost_half;
          (nomin = zero sort) --> f coef lost_zero;
          anything_else --> (f coef lost_afew);
        ]
      else
        ite (nomin > denom) (mask_bit sort i) (zero sort) >>>= fun bit ->
        ite (nomin > denom) (nomin - denom) nomin >>>= fun next_nomin ->
        next_nomin lsl one sort >>>= fun next_nomin ->
        bind next_nomin (fun next_nomin ->
            loop Caml.(i - 1) (bit :: bits) next_nomin) in
    loop Caml.(prec - 1) [] !!nomin

  let fdiv_finite fsort rm x y  =
    let open B in
    let norm_nominator exps sigs nomin denom f =
      ite (nomin < denom) (nomin lsl one sigs) nomin >>>= fun nomin' ->
      ite (nomin < denom) (one exps) (zero exps) >>>= fun dexpn ->
      f nomin' dexpn in
    let exps,sigs = floats fsort in
    let prec = precision fsort in
    let prec' = of_int exps prec in
    let maxe = max_exponent exps in
    let mine = min_exponent exps in
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun ysign yexpn ycoef ->
    xor xsign ysign >>>= fun sign ->
    normalize_coef xcoef @@ fun nomin dx ->
    normalize_coef ycoef @@ fun denom dy ->
    dy - dx >>>= fun de ->
    bitv_extend nomin ~addend:1 @@ fun nomin sigs' ->
    bitv_extend denom ~addend:1 @@ fun denom _ ->
    norm_nominator exps sigs' nomin denom @@ fun nomin dexpn' ->
    long_division prec nomin denom @@ fun coef (guard', round', sticky') ->
    unsigned sigs coef >>>= fun coef ->
    of_int exps (bias fsort) >>>= fun bias ->
    unsigned exps de - dexpn' >>>= fun dexpn ->
    xexpn - yexpn >>>= fun expn ->
    ((xexpn < yexpn) && (yexpn - xexpn > dexpn + bias - mine)) >>>= fun underflowed ->
    ((xexpn < yexpn) && (yexpn - xexpn > prec' + dexpn + bias - mine)) >>>= fun is_underflow ->
    ((xexpn > yexpn) && (expn > maxe - dexpn - bias)) >>>= fun is_overflow ->
    expn + dexpn + bias >>>= fun expn ->
    if_ underflowed
      ~then_:(
        abs expn + mine >>>= fun fix_underflow ->
        extract_last coef fix_underflow >>>= fun loss ->
        guardbits loss fix_underflow @@ fun guard round sticky ->
        (sticky || round' || sticky') >>>= fun sticky ->
        coef lsr fix_underflow >>>= fun coef ->
        is_round_up rm sign (lsb coef) guard round sticky >>>= fun up ->
        ite up (succ coef) coef >>>= fun coef ->
        norm mine coef @@ fun expn coef -> pack fsort sign expn coef)
      ~else_:(
        is_round_up rm sign (lsb coef) guard' round' sticky' >>>= fun up ->
        ite up (succ coef) coef >>>= fun coef ->
        ite is_underflow (zero exps) expn >>>= fun expn ->
        ite is_overflow  (ones exps) expn >>>= fun expn ->
        ite (is_overflow || is_underflow) (zero sigs) coef >>>= fun coef ->
        norm expn coef @@ fun expn coef -> pack fsort sign expn coef)

  let fdiv_special fsort x y =
    let open B in
    with_special fsort x @@ fun ~is_inf:xinf ~is_snan:xsnan ~is_qnan:xqnan ->
    with_special fsort y @@ fun ~is_inf:yinf ~is_snan:ysnan ~is_qnan:_yqnan ->
    fsign x >>>= fun xsign ->
    fsign y >>>= fun ysign ->
    (xinf && yinf) >>>= fun is_inf ->
    inv (xinf || xsnan || xsnan) >>>= fun is_finx ->
    inv (yinf || ysnan || ysnan) >>>= fun is_finy ->
    xor xsign ysign >>>= fun sign ->
    match_ [
      (is_zero x && is_zero y) --> qnan fsort;
      (is_zero x && is_finy) --> fzero fsort sign;
      (is_zero y && is_finx) --> inf fsort sign;
      (xinf && yinf) --> qnan fsort;
      is_inf --> with_sign (xor xsign ysign) x;
      (xsnan || xqnan) --> transform_to_quite fsort x;
      anything_else --> (transform_to_quite fsort y);
    ]

  let fdiv fsort rm x y =
    let open B in
    ite (is_finite_nonzero fsort x && is_finite_nonzero fsort y)
      (fdiv_finite fsort rm x y)
      (fdiv_special fsort x y)

  let ftwo fsort =
    fone fsort B.b0 >>>= fun one ->
    fadd fsort B.rne one one

  (* pre: fsort ">=" fsort' *)
  let truncate fsort x rm fsort' =
    let open B in
    let sigs_sh = Bits.size (sigs fsort') in
    let d_bias = Caml.(bias fsort - bias fsort') in
    let dst_maxe = max_exponent' (Bits.size @@ exps fsort') in
    unpack fsort x @@ fun sign expn coef ->
    if_ (is_all_ones expn || is_zero expn)
      ~then_:(
        low (exps fsort') expn >>>= fun expn ->
        high (sigs fsort') coef >>>= fun coef ->
        pack fsort' sign expn coef)
      ~else_:(
        expn - of_int (exps fsort) d_bias >>>= fun expn ->
        if_ (expn > of_int (exps fsort) dst_maxe)
          ~then_:(inf fsort' sign)
          ~else_:(
            low (exps fsort') expn >>>= fun expn ->
            coef lsl of_int (sigs fsort) sigs_sh >>>= fun truncated ->
            high (sigs fsort') truncated >>>= fun truncated ->
            high (sigs fsort') coef >>>= fun coef ->
            lsb coef >>>= fun last ->
            msb truncated >>>= fun guard ->
            truncated lsl one (sigs fsort') >>>= fun truncated ->
            msb truncated >>>= fun round ->
            truncated lsl one (sigs fsort') >>>= fun truncated ->
            non_zero truncated >>>= fun sticky ->
            is_round_up rm sign last guard round sticky >>>= fun up ->
            ite (is_all_ones coef && up) (succ expn) expn >>>= fun expn ->
            ite up (succ coef) coef >>>= fun coef ->
            ite (is_all_ones expn) (inf fsort' sign)
              (pack fsort' sign expn coef)))

  (* pre: fsort "<=" fsort' *)
  let extend fsort x fsort' =
    let open B in
    let d_sigs = Caml.(Bits.size (sigs fsort') - Bits.size (sigs fsort)) in
    let d_bias = Caml.(bias fsort' - bias fsort) in
    unpack fsort x @@ fun sign expn coef ->
    match_ [
      is_all_ones expn --> ones (exps fsort');
      (expn = min_exponent (exps fsort)) --> min_exponent (exps fsort');
      anything_else -->
      (unsigned (exps fsort') expn + of_int (exps fsort') d_bias);
    ] >>>= fun expn ->
    unsigned (sigs fsort') coef >>>= fun coef ->
    (coef lsl (of_int (sigs fsort') d_sigs)) >>>= fun coef ->
    pack fsort' sign expn coef

  let convert fsort x rm fsort' =
    let size f = Bits.size (IEEE754.Sort.bits f) in
    if size fsort = size fsort' then
      B.unsigned (bits fsort') x
    else if size fsort < size fsort' then
      extend fsort x fsort'
    else truncate fsort x rm fsort'

  let double_precision fsort =
    let bits = Bits.size (bits fsort) in
    let bits' = 2 * bits in
    let p = Option.value_exn (IEEE754.binary bits') in
    IEEE754.Sort.define p

  let gen_cast_float fsort _rmode sign bitv =
    let open IEEE754 in
    let open B in
    let {p;bias} = Sort.spec fsort in
    let exps = exps fsort in
    let sigs = Bits.define p in
    bitv >>-> fun inps bitv ->
    of_int exps Caml.(bias + p - 1) >>>= fun expn ->
    of_int sigs p >>>= fun prec ->
    clz !!bitv >>>= fun clz ->
    unsigned sigs clz >>>= fun clz ->
    of_int sigs (Bits.size inps) - clz >>>= fun msbn ->
    if_ (msbn > prec)
      ~then_:(msbn - prec >>>= fun de ->
              msbn - one sigs >>>= fun hi ->
              hi - prec + one sigs >>>= fun lo ->
              extract sigs hi lo !!bitv >>>= fun coef ->
              extract sigs (pred lo) (zero sigs) !!bitv >>>= fun loss ->
              round rne sign coef loss lo @@ fun coef rnd_overflow ->
              ite rnd_overflow (not (ones sigs lsr one sigs)) coef >>>= fun coef ->
              ite rnd_overflow (succ expn) expn >>>= fun expn ->
              expn + unsigned exps de >>>= fun expn ->
              norm expn coef @@ fun expn coef ->
              pack fsort sign expn coef)
      ~else_:(unsigned sigs !!bitv >>>= fun coef ->
              norm expn coef @@ fun expn coef ->
              pack fsort sign expn coef)

  let cast_float fsort rmode bitv = gen_cast_float fsort rmode B.b0 bitv

  let cast_float_signed fsort rmode bitv =
    let open B in
    let sign = msb bitv in
    let bitv = ite sign (neg bitv) bitv in
    gen_cast_float fsort rmode sign bitv

  let cast_int fsort outs bitv =
    let open B in
    let open IEEE754 in
    let {p;bias} = Sort.spec fsort in
    let exps = exps fsort in
    let sigs = Bits.define p in
    unpack fsort bitv @@ fun sign expn coef ->
    expn - of_int exps bias + one exps >>>= fun bits ->
    of_int sigs p - unsigned sigs bits >>>= fun bits ->
    coef lsr unsigned sigs bits >>>= fun coef ->
    unsigned outs coef >>>= fun coef ->
    ite sign (neg coef) coef

  (* returns x in range [1.0; 2.0] *)
  let range_reduction fsort x f =
    let open B in
    let bias = bias fsort in
    let low = of_int (exps fsort) Caml.(bias - 0) in
    let top = of_int (exps fsort) Caml.(bias + 1) in
    unpack_raw fsort x @@ fun sign expn coef ->
    match_ [
      (expn = top && non_zero coef) --> (expn - low);
      (expn > top) --> (expn - low);
      (expn = low || expn = top) --> zero (exps fsort);
      (expn < low) --> (low - expn);
    ] >>>= fun d_expn ->
    (expn >= low) >>>= fun increase ->
    ite (expn >= low) (expn - d_expn) (expn + d_expn) >>>= fun expn ->
    pack_raw fsort sign expn coef >>>= fun r -> f r d_expn increase

  let sqrt2 fsort' =
    let open IEEE754 in
    let fsort = Sort.define binary128 in
    B.int (bits fsort)
      (Bitvec.of_string "0x3fff6a09e667f3bcc908b2fb1366ea95") >>>= fun x ->
    convert fsort x B.rne fsort'

  let range_reconstruction fsort x d_expn clz increase =
    let idiv = fdiv in
    let open B in
    sqrt2 fsort >>>= fun sqrt2 ->
    unpack_raw fsort x @@ fun sign expn coef ->
    if_ (is_zero clz)
      ~then_:(
        lsb d_expn >>>= fun is_odd ->
        ite (is_odd && increase) (succ d_expn)
          (ite is_odd (pred d_expn) d_expn) >>>= fun d_expn ->
        ite increase
          (expn + d_expn / of_int (exps fsort) 2)
          (expn - d_expn / of_int (exps fsort) 2) >>>= fun expn ->
        pack_raw fsort sign expn coef >>>= fun r ->
        ite is_odd (idiv fsort rne r sqrt2) r)
      ~else_:( (* for subnormals  *)
        unsigned (exps fsort) clz >>>= fun clz ->
        lsb clz >>>= fun is_odd ->
        ite is_odd (pred clz) clz >>>= fun clz ->
        clz / of_int (exps fsort) 2 >>>= fun clz ->
        expn - d_expn / of_int (exps fsort) 2 - clz >>>= fun expn ->
        pack_raw fsort sign expn coef >>>= fun r ->
        ite is_odd (idiv fsort rne r sqrt2) r)

  let horner fsort x cfs =
    let open B in
    let ( + ) = add_or_sub_finite b0 fsort rne in
    let ( * ) = fmul_finite fsort rne in
    let rec sum y = function
      | [] -> y
      | c :: cs ->
        x * y >>>= fun y ->
        y + c >>>= fun y ->
        sum y cs in
    match cfs with
    | [] -> assert false
    | [c] -> c
    | c::cs -> sum c cs

  let normalize_subnormal fsort x f =
    unpack fsort x @@ fun sign expn coef ->
    clz coef >>>= fun clz ->
    B.(coef lsl clz) >>>= fun coef ->
    pack fsort sign expn coef >>>= fun r -> f r clz

  (* x + 1 for [0.0;1.0] and degree = 23 *)
  let coefs s =
    List.map ~f:(fun x -> B.int s @@ Bitvec.of_string x) [
      "0x3fea283d1af5d3b8b0e9889579874140";
      "0xbfede2119c815a1d59de530775f68488";
      "0x3ff0766a0fd57550fbcaf5949c387bfb";
      "0xbff272ebd07e2237538440a72429c8d9";
      "0x3ff408b110927387f462771163341fec";
      "0xbff5242b7488a8af2abfbadce8148c9e";
      "0x3ff606758477172056f0e80bc6f46e09";
      "0xbff69084710b6e59fb3f53199edfb3dd";
      "0x3ff70e24e277a45deb05c37590719576";
      "0xbff74ec376fd27c8ac279836961f6025";
      "0x3ff78aabb74359ac28498e73a2e9989f";
      "0xbff7c74981d8ac6e123f062aee10b24c";
      "0x3ff805e10b3ea64c491b21abdb7f9796";
      "0xbff82fbf8642c69d9c901bcfcd18b8e1";
      "0x3ff8657a1d1b03f0f63a64018888f16c";
      "0xbff8acff29ed5e34f19ffa86dc9e5b9a";
      "0x3ff907fff45a4ee1cba42e6e42229c85";
      "0xbff94fffff0ee79b4a473fa1503182d5";
      "0x3ff9bffffff219963def0395eb8c25e4";
      "0xbffa3fffffffbb055e0eddae1c1de75f";
      "0x3ffafffffffffe5a8a7bc5d57eea900e";
      "0xbffbfffffffffffd5373ee69a938caf9";
      "0x3ffdffffffffffffff22a39a2a24d3f4";
      "0x3fff0000000000000000000000000000";
    ]

  let fsqrt fsort rm x =
    let fsort' = double_precision fsort in
    B.ite (is_zero x) (fzero fsort B.b0)
      (normalize_subnormal fsort x @@ fun x clz ->
       range_reduction fsort x @@ fun y d_expn increase ->
       fone fsort B.b0 >>>= fun one ->
       fsub fsort rm y one >>>= fun y ->
       extend fsort y fsort' >>>= fun y ->
       horner fsort' y (coefs (bits fsort')) >>>= fun y ->
       B.unsigned (exps fsort') d_expn >>>= fun d_expn ->
       range_reconstruction fsort' y d_expn clz increase >>>= fun r ->
       truncate fsort' r rm fsort)

  let test fsort x =
    let fsort' = double_precision fsort in
    normalize_subnormal fsort x @@ fun x clz ->
    range_reduction fsort x @@ fun y d_expn increase ->
    fone fsort B.b0 >>>= fun one ->
    fsub fsort B.rne y one >>>= fun y ->
    extend fsort y fsort' >>>= fun y ->
    horner fsort' y (coefs (bits fsort')) >>>= fun y ->
    B.unsigned (exps fsort') d_expn >>>= fun d_expn ->
    range_reconstruction fsort' y d_expn clz increase >>>= fun r ->
    truncate fsort' r B.rne fsort
end
