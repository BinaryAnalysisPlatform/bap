open Core_kernel
open Bap.Std

open Bap_knowledge
open Bap_core_theory
open Knowledge.Syntax

type rounding =
  | Nearest_even  (** round to nearest, ties to even *)
  | Nearest_away  (** round to nearest, ties to away *)
  | Towards_zero  (** round toward zero              *)
  | Positive_inf  (** round toward positive infinity *)
  | Negative_inf  (** round toward negative infinity *)
[@@deriving sexp]

type 'a t = 'a knowledge

type ('b, 'e, 't, 's) unop =
  ((('b,'e,'t) IEEE754.t,'s) format float sort ->
   rmode value t -> 's bitv value t -> 's bitv value t)


type ('b, 'e, 't, 's) binop =
  ((('b,'e,'t) IEEE754.t,'s) format float sort ->
   rmode value t -> 's bitv value t -> 's bitv value t -> 's bitv value t)

module Rounding = struct

  module Rounding_domain = struct
    open Domain.Order

    type t = rounding option

    let empty = None

    let partial x y : Domain.Order.partial = match x,y with
      | None,None -> EQ
      | None,_ -> LE
      | _,None -> GE
      | _ -> NC

    let inspect t = failwith "unimplemented"
  end

  let rounding = Semantics.declare ~name:"rounding" (module Rounding_domain)

  module T = struct
    let make t = !! (Value.put rounding (Value.empty Rmode.t) (Some t))

    let rne = make Nearest_even
    let rna = make Nearest_away
    let rtp = make Towards_zero
    let rtn = make Positive_inf
    let rtz = make Negative_inf
  end

  let get rm =
    rm >>= fun r ->
    match Value.get rounding r with
    | None -> !! Nearest_even
    | Some r -> !! r
end

module Make(B : Theory.Basic) = struct

  include Rounding.T

  open Knowledge.Syntax

  module B = struct
    include B

    let one s = succ (zero s)
    let ones s = B.(not (zero s))

    let is_one x =
      x >>= fun v -> eq x (one (Value.sort v))

    let is_all_ones x =
      x >>= fun v -> eq x (ones (Value.sort v))

    let of_int sort v =
      int sort (Word.of_int ~width:(Bits.size sort) v)

    let is_negative = B.msb
    let is_positive x = B.inv (B.msb x)
    let is_non_negative x = B.or_ (is_positive x) (B.is_zero x)
    let abs x = ite (is_negative x) (neg x) x

    let of_bool = function
      | true -> b1
      | false -> b0

    let testbit x i = lsb (rshift x i)

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
  end

  module Lost = struct
    let alot s = B.of_int s 0b11
    let half s = B.of_int s 0b10
    let zero s = B.of_int s 0b00
    let afew s = B.of_int s 0b01
    let bits s = B.of_int s 2
  end

  type 'a t = 'a knowledge

  let (>>->) x f =
    x >>= fun x ->
    f x (Value.sort x)

  let empty dom sort x = !! (Value.put dom (Value.empty sort) x)

  let bind a body =
    a >>= fun a ->
    let sort = Value.sort a in
    Var.Generator.fresh sort >>= fun v ->
    B.let_ v !!a (body (B.var v))

  let (>=>) = bind

  let exps = IEEE754.Sort.exps

  let sigs fsort =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    Bits.define spec.p

  let floats fsort = exps fsort, sigs fsort

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

  (* TODO: refactor it a little  *)
  let pack fsort sign expn coef =
    let open B in
    let {IEEE754.p; t} = IEEE754.Sort.spec fsort in
    ite ((inv (msb coef)) && (is_one expn)) (pred expn) expn >=> fun expn ->
    if Caml.(p = t) then pack_raw fsort sign expn coef
    else
      B.low (Bits.define t) coef >=> fun coef ->
      pack_raw fsort sign expn coef >>= fun r -> !!r

  let raw_significand fsort bitv =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    let sigs = Bits.define spec.t in
    B.low sigs bitv

  let finite_significand fsort expn bitv =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    raw_significand fsort bitv >=> fun coef ->
    if spec.t = spec.p then coef
    else
      let bit = Bits.define 1 in
      let leading_bit = B.(ite (is_zero expn) (zero bit) (one bit)) in
      B.append (sigs fsort) leading_bit coef

  let unpack fsort x f =
    x >=> fun x ->
    exponent fsort x >=> fun expn ->
    fsign x >=> fun sign ->
    finite_significand fsort expn x >=> fun coef ->
    B.(ite (is_zero expn) (succ expn) expn) >=> fun expn ->
    f sign expn coef

  let unpack_raw fsort x f =
    x >=> fun x ->
    exponent fsort x >=> fun expn ->
    raw_significand fsort x >=> fun coef ->
    f (fsign x) expn coef

  let with_sign sign bitv =
    let open B in
    bitv >>-> fun bitv s ->
    let s' = Bits.define Caml.(Bits.size s - 1) in
    let bit = Bits.define 1 in
    ite sign ((append s (one bit) (zero s')) lor !!bitv)
      ((append s (zero bit) (ones s')) land !!bitv)

  let fzero fsort sign =
    let open B in
    let bits = IEEE754.Sort.bits fsort in
    zero bits >=> fun bitv ->
    ite sign (
        ones bits >=> fun ones ->
        not (ones lsr one bits) >=> fun one ->
        one lor bitv) bitv

  let fone fsort =
    let open IEEE754 in
    let {bias; t} = Sort.spec fsort in
    let expn = B.of_int (Sort.exps fsort) bias in
    let sigs = Bits.define t in
    pack_raw fsort B.b0 expn (B.zero sigs)

  let inf fsort sign =
    let open B in
    let exps = IEEE754.Sort.exps fsort in
    pack fsort sign (B.ones exps) (B.zero (sigs fsort))

  let is_inf fsort x =
    unpack fsort x @@ fun _ expn coef ->
    B.(and_ (is_zero coef) (is_all_ones expn))

  let is_pinf fsort x =
    is_inf fsort x >=> fun is_inf ->
    B.(and_ is_inf (inv (msb x)))

  let is_ninf fsort x =
    is_inf fsort x >=> fun is_inf ->
    B.(and_ is_inf (msb x))

  let is_qnan fsort x =
    let open B in
    unpack_raw fsort x @@ fun sign expn coef ->
    is_all_ones expn && non_zero coef && msb coef

  let is_snan fsort x =
    let open B in
    unpack_raw fsort x @@ fun sign expn coef ->
    is_all_ones expn && non_zero coef && inv (msb coef)

  let is_nan fsort x =
    let open B in
    unpack_raw fsort x @@ fun sign expn coef ->
    is_all_ones expn && non_zero coef

  let qnan fsort =
    let open B in
    let open IEEE754 in
    let exps = Sort.exps fsort in
    let spec = Sort.spec fsort in
    let sigs = Bits.define spec.t in
    not (ones sigs lsr one sigs) >=> fun coef ->
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
    unpack_raw fsort x @@ fun sign expn coef ->
    is_all_ones expn >=> fun is_special ->
    (is_special && is_zero coef) >=> fun is_inf ->
    (is_special && non_zero coef && inv (msb coef)) >=> fun is_snan ->
    (is_special && msb coef) >=> fun is_qnan ->
    f ~is_inf ~is_snan ~is_qnan

  let is_special fsort x = unpack fsort x @@ fun _ expn _ -> B.is_all_ones expn
  let is_finite fsort x = B.inv (is_special fsort x)
  let is_finite_nonzero fsort x =
    let open B in
    unpack_raw fsort x @@ fun _ expn coef ->
    inv (is_all_ones expn) >=> fun ok_expn ->
    ok_expn && (non_zero expn || non_zero coef)

  let is_norml fsort x = unpack_raw fsort x @@ fun _ e _ -> B.non_zero e
  let is_subnormal fsort x =
    unpack_raw fsort x @@ fun _ e _ -> B.is_zero e

  let is_zero x =
    let open B in
    x >>-> fun x s ->
    is_zero ((!!x lsl one s) lsr one s)

  let precision fsort = let {IEEE754.p} = IEEE754.Sort.spec fsort in p
  let bias fsort = let {IEEE754.bias} = IEEE754.Sort.spec fsort in bias

  let match_ cases ~default =
    let cases = List.rev cases in
    List.fold cases ~init:default
      ~f:(fun fin (cond, ok) -> B.ite cond ok fin)

  let (-->) x y = x,y

  let extract_last x n =
    let open B in
    x >>-> fun x sort ->
    let mask = not (ones sort lsl n)  in
    !!x land mask

  let is_round_up rm sign coef guard round sticky =
    let open B in
    Rounding.get rm >>= fun rm ->
    match rm with
    | Positive_inf -> inv sign
    | Negative_inf -> sign
    | Nearest_away -> guard
    | Nearest_even -> guard && (round || sticky || lsb coef)
    | Towards_zero -> b0

  let guardbit_of_loss loss lost_bits =
    let open B in
    lost_bits >>-> fun lost_bits sort ->
    non_zero !!lost_bits >=> fun is_valid ->
    ite is_valid (pred !!lost_bits) (zero sort) >=> fun pos ->
    ite is_valid (testbit loss pos) b0

  let roundbit_of_loss loss lost_bits =
    let open B in
    lost_bits >>-> fun lost_bits sort ->
    (!!lost_bits > one sort) >=> fun is_valid ->
    ite is_valid (pred !!lost_bits) (zero sort) >=> fun pos ->
    ite is_valid (testbit loss (pred pos)) b0

  let stickybit_of_loss loss lost_bits =
    let open B in
    lost_bits >>-> fun lost_bits sort ->
    of_int sort 3 >=> fun minbits ->
    (!!lost_bits >= minbits) >=> fun is_valid ->
    ite is_valid (pred (pred !!lost_bits)) (zero sort) >=> fun n ->
    ite is_valid (non_zero (extract_last loss n)) b0

  (* the result of rounding must be checked because in the corner case
     when input is 111...1 it could return 000..0, that means that
     exponent must be incremented and msb of coef set to 1 *)
  let round rm sign coef loss lost_bits f =
    let open Rmode in
    let open B in
    guardbit_of_loss loss lost_bits >=> fun guard ->
    roundbit_of_loss loss lost_bits >=> fun round ->
    stickybit_of_loss loss lost_bits >=> fun sticky ->
    ite (is_round_up rm sign coef guard round sticky) (succ coef) coef >=> fun coef' ->
    and_ (non_zero coef) (is_zero coef') >=> fun is_overflow ->
    f coef' is_overflow

  (* maximum possible exponent that fits in [n - 1] bits. (one for sign)
     and one for special numbers like inf or nan *)
  let max_exponent' n = int_of_float (2.0 ** (float_of_int n )) - 2
  let min_exponent' n = 1
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
    x >>-> fun x sort ->
    let size = Bits.size sort in
    let pow, num = nearest_pow2 size in
    let sort' = Bits.define num in
    let shifts = List.init pow ~f:(fun p -> num / (2 lsl p)) in
    let shifts,_ =
      List.fold shifts ~init:([],0)
        ~f:(fun (acc,prev) curr ->
            let total = curr + prev in
            (total, curr) :: acc, total) in
    let shifts = List.rev shifts in
    let rec loop lets x = function
      | [] -> List.fold lets ~f:B.(+) ~init:(B.zero sort)
      | (total, shift) :: shifts ->
        B.of_int sort shift >=> fun shift ->
        B.of_int sort total >=> fun total ->
        B.(ones sort' lsl total) >=> fun mask ->
        B.(ite (is_zero (x land mask)) (x lsl shift) x) >=> fun nextx ->
        B.(ite (is_zero (x land mask)) shift (B.zero sort)) >=> fun nextn ->
        loop (nextn :: lets) nextx shifts in
    loop [] (B.unsigned sort' !!x) shifts >=> fun n ->
    B.of_int sort (num - size) >=> fun dif ->
    B.ite (B.is_zero !!x) (B.of_int sort size) B.(n - dif)

  let possible_lshift expn coef =
    let open B in
    expn >>-> fun expn exps ->
    coef >>-> fun coef sigs ->
    clz !!coef >=> fun clz ->
    min_exponent exps >=> fun mine ->
    ite (!!expn < mine) (zero exps) (!!expn - mine) >=> fun diff ->
    unsigned sigs diff >=> fun diff ->
    ite (clz < diff) clz diff

  let norm_finite expn coef f =
    let open B in
    expn >>-> fun expn exps ->
    coef >>-> fun coef sigs ->
    possible_lshift !!expn !!coef >=> fun shift ->
    unsigned exps shift >=> fun dexpn ->
    !!coef lsl shift >=> fun coef ->
    ite (is_zero coef) (min_exponent exps) (!!expn - dexpn) >=> fun expn ->
    f expn coef

  let norm expn coef f =
    let open B in
    expn >>-> fun expn exps ->
    ite (is_all_ones !!expn) (f !!expn coef)
      (norm_finite !!expn coef f)

  let msbn x =
    let open B in
    x >>-> fun x sort ->
    clz !!x >=> fun clz ->
    of_int sort (Bits.size sort) - clz - one sort

  let xor s s' = B.(and_ (or_ s s') (inv (and_ s s')))

  let leading_one sort =
    B.(one sort lsl (of_int sort Caml.(Bits.size sort - 1)))

  let fadd_raw fsort rm sign xexpn xcoef yexpn ycoef f =
    let open B in
    let exps, sigs = floats fsort in
    ite (xexpn > yexpn) (xexpn - yexpn) (yexpn - xexpn) >=> fun lost_bits ->
    match_ [
        (xexpn = yexpn) --> zero sigs;
        (xexpn > yexpn) --> extract_last ycoef lost_bits;
      ] ~default:(extract_last xcoef lost_bits) >=> fun loss ->
    ite (xexpn > yexpn) xcoef (xcoef lsr lost_bits) >=> fun xcoef ->
    ite (yexpn > xexpn) ycoef (ycoef lsr lost_bits) >=> fun ycoef ->
    xcoef + ycoef >=> fun sum ->
    max xexpn yexpn >=> fun expn ->
    ite (sum >= xcoef) expn (succ expn) >=> fun expn ->
    guardbit_of_loss loss lost_bits >=> fun guard' ->
    roundbit_of_loss loss lost_bits >=> fun round' ->
    stickybit_of_loss loss lost_bits >=> fun sticky' ->
    ite (sum < xcoef) (lsb sum) guard' >=> fun guard ->
    ite (sum < xcoef) guard' round' >=> fun round ->
    ite (sum < xcoef) (round' || sticky') sticky' >=> fun sticky ->
    ite (sum < xcoef) (sum lsr one sigs) sum >=> fun coef ->
    ite (sum < xcoef) (coef lor leading_one sigs) coef >=> fun coef ->
    is_round_up rm sign coef guard round sticky >=> fun up ->
    ite up (succ coef) coef >=> fun coef' ->
    (is_zero coef' && non_zero coef) >=> fun rnd_overflow ->
    ite rnd_overflow (leading_one sigs) coef' >=> fun coef ->
    ite rnd_overflow (succ expn) expn >=> fun expn' ->
    ite (expn > max_exponent exps) (zero sigs) coef >=> fun coef ->
    norm expn coef @@ fun expn coef ->
    expn >>= fun expn ->
    coef >>= fun coef ->
    f sign !!expn !!coef

  let fadd_finite fsort rm x y =
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun _ysign yexpn ycoef ->
    fadd_raw fsort rm xsign xexpn xcoef yexpn ycoef  @@ fun sign expn coef ->
    pack fsort sign expn coef

  let extend bitv ~addend f =
    bitv >>-> fun bitv sort ->
    let sort' = Bits.define (Bits.size sort + addend) in
    B.unsigned sort' !!bitv >=> fun bitv ->
    f bitv sort'

  let common_ground xexpn xcoef yexpn ycoef f =
    let open B in
    xexpn >>-> fun xexpn exps ->
    xcoef >>-> fun xcoef sigs ->
    ite (!!xexpn > yexpn) (!!xexpn - yexpn) (yexpn - !!xexpn) >=> fun diff ->
    ite (is_zero diff) diff (diff - one exps) >=> fun lost_bits ->
    match_ [
        (!!xexpn > yexpn) --> (
          extract_last ycoef lost_bits >=> fun loss ->
          !!xcoef lsl one sigs >=> fun xcoef ->
          ycoef lsr lost_bits >=> fun ycoef ->
          f loss lost_bits xcoef ycoef);
        (yexpn > !!xexpn) --> (
          extract_last !!xcoef lost_bits >=> fun loss ->
          !!xcoef lsr lost_bits >=> fun xcoef ->
          ycoef lsl one sigs >=> fun ycoef ->
          f loss lost_bits xcoef ycoef);
      ] ~default:(f (zero sigs) (zero exps) !!xcoef ycoef)

  let fsub_raw fsort rm xsign xexpn xcoef yexpn ycoef f =
    let open B in
    let exps, sigs = floats fsort in
    extend xcoef ~addend:1 @@ fun xcoef sigs' ->
    extend ycoef ~addend:1 @@ fun ycoef _ ->
    or_ (xexpn < yexpn) (and_ (xexpn = yexpn) (xcoef < ycoef)) >=> fun swap ->
    ite swap (inv xsign) xsign >=> fun sign ->
    common_ground xexpn xcoef yexpn ycoef @@ fun loss lost_bits xcoef ycoef ->
    ite (is_zero loss) (zero sigs') (one sigs') >=> fun borrow ->
    ite swap (ycoef - xcoef - borrow) (xcoef - ycoef - borrow) >=> fun coef ->
    msb coef >=> fun msbc ->
    max xexpn yexpn >=> fun expn ->
    ite (xexpn = yexpn) expn (expn - one exps) >=> fun expn ->
    ite (is_zero coef) (min_exponent exps) (ite msbc (succ expn) expn) >=> fun expn ->
    guardbit_of_loss loss lost_bits  >=> fun guard' ->
    roundbit_of_loss loss lost_bits  >=> fun round' ->
    stickybit_of_loss loss lost_bits >=> fun sticky' ->
    ite (round' || sticky') (inv guard') guard' >=> fun guard' ->
    ite msbc (lsb coef) guard' >=> fun guard ->
    ite msbc guard' round' >=> fun round ->
    ite msbc (round' || sticky')  sticky' >=> fun sticky ->
    ite msbc (coef lsr one sigs') coef >=> fun coef ->
    is_round_up rm sign coef guard round sticky >=> fun up ->
    unsigned sigs coef >=> fun coef ->
    ite up (succ coef) coef >=> fun coef' ->
    (is_zero coef' && non_zero coef) >=> fun rnd_overflow ->
    ite rnd_overflow (leading_one sigs) coef' >=> fun coef ->
    ite rnd_overflow (succ expn) expn >=> fun expn' ->
    norm expn coef @@ fun expn coef -> f sign expn coef

  let add_or_sub_raw fsort is_sub rm xsign xexpn xcoef ysign yexpn ycoef f =
    let open B in
    let ( lxor ) = xor in
    let is_sub = is_sub lxor (xsign lxor ysign) in
    ite is_sub (fsub_raw fsort rm xsign xexpn xcoef yexpn ycoef f)
    (fadd_raw fsort rm xsign xexpn xcoef yexpn ycoef f)

  let add_or_sub_finite is_sub fsort rm x y =
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun ysign yexpn ycoef ->
    add_or_sub_raw fsort is_sub rm xsign xexpn xcoef ysign yexpn ycoef @@
      fun sign expn coef -> pack fsort sign expn coef

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
    (xsnan || xqnan) >=> fun xnan ->
    (ysnan || yqnan) >=> fun ynan ->
    (xinf && yinf) >=> fun is_inf ->
       match_ [
       (is_sub && is_inf) --> qnan fsort;
       (xinf && yinf) --> x;
       (xnan && (not ynan)) --> transform_to_quite fsort x;
       (ynan && (not xnan)) --> transform_to_quite fsort y;
    ] ~default:(transform_to_quite fsort x)

  let add_or_sub ~is_sub fsort rm x y =
    let open B in
    ite (is_finite fsort x && is_finite fsort y)
      (add_or_sub_finite is_sub fsort rm x y)
      (fsum_special fsort is_sub x y)

  let fsub fsort rm x y = add_or_sub ~is_sub:B.b1 fsort rm x y
  let fadd fsort rm x y = add_or_sub ~is_sub:B.b0 fsort rm x y

  let double bitv f =
    bitv >>-> fun bitv sort ->
    extend !!bitv ~addend:(Bits.size sort) f

  let normalize_coef coef f =
    let open B in
    coef >>-> fun coef sort ->
    clz !!coef >=> fun clz ->
    !!coef lsl clz >=> fun coef ->
    f coef clz

  (* TODO: handle rounding overflow *)
  (* TODO: finish with comments *)
  (* TODO: refactor it (probably cover with more tests before) *)
  (* Multiplication.
     The result of (Sx,Ex,Cx) * (Sy,Ey,Cy) is (Sx xor Sy, Ex + Ey - bias, Cx * Cy),
     where S,E,C - sign, exponent and coefficent.
     Also, say we have 53-bit precision: C = c52 . c51 . c50 ... c0.
     The result is C = c105 . c104 ... c0, where result is in first 53
     bits.  *)
  let fmul_raw fsort rm xsign xexpn xcoef ysign yexpn ycoef f =
    let open B in
    let double e c f =
      double e @@ fun e es ->
      double c @@ fun c cs ->
      f e es c cs in
    let precision = precision fsort in
    let maxe = max_exponent (exps fsort) in
    let exps,sigs = floats fsort in
    xor xsign ysign >=> fun sign ->
    normalize_coef xcoef @@ fun xcoef dx ->
    normalize_coef ycoef @@ fun ycoef dy ->
    dx + dy >=> fun dnorm ->
    double xexpn xcoef @@ fun xexpn exps' xcoef sigs' ->
    double yexpn ycoef @@ fun yexpn _ ycoef _ ->
    of_int sigs' precision >=> fun prec ->
    xexpn + yexpn >=> fun expn ->
    xcoef * ycoef >=> fun coef ->
    msb coef >=> fun coef_overflowed ->
    ite coef_overflowed (succ expn) expn >=> fun expn ->
    of_int exps' (bias fsort) >=> fun bias ->
    bias + unsigned exps' dnorm >=> fun dexpn ->
    ite (dexpn >=$ expn) (dexpn - expn + min_exponent exps') (zero exps') >=> fun underflow ->
    underflow > of_int exps' precision >=> fun is_underflow ->
    expn - dexpn + underflow >=> fun expn ->
    expn > unsigned exps' maxe >=> fun is_overflow ->
    ite coef_overflowed (one sigs') (zero sigs') >=> fun from_overflow ->
    coef lsr (from_overflow + unsigned sigs' underflow) >=> fun coef ->

    coef lsl one sigs' >=> fun coef ->
    low sigs coef >=> fun loss ->
    high sigs coef >=> fun coef ->
    round rm sign coef loss prec @@ fun coef rnd_overflow ->
    low exps expn >=> fun expn ->
    norm expn coef @@ fun expn coef ->

    ite is_underflow (zero exps) expn >=> fun expn ->
    ite is_overflow (ones exps) expn >=> fun expn ->
    ite (is_overflow || is_underflow) (zero sigs) coef >=> fun coef ->
    f sign expn coef

  let fmul_finite fsort rm x y =
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun ysign yexpn ycoef ->
    fmul_raw fsort rm xsign xexpn xcoef ysign yexpn ycoef @@
    fun sign expn coef -> pack fsort sign expn coef

  let fmul_special fsort x y =
    let open B in
    with_special fsort x @@ fun ~is_inf:xinf ~is_snan:xsnan ~is_qnan:xqnan ->
    with_special fsort y @@ fun ~is_inf:yinf ~is_snan:ysnan ~is_qnan:yqnan ->
    fsign x >=> fun xsign ->
    fsign y >=> fun ysign ->
    (xinf && yinf) >=> fun is_inf ->
    match_ [
      (is_zero x && yinf) --> qnan fsort;
      (is_zero y && xinf) --> qnan fsort;
      is_inf --> with_sign (xor xsign ysign) x;
      (xsnan || xqnan) --> transform_to_quite fsort x
    ] ~default:(transform_to_quite fsort y)

  let fmul fsort rm x y =
    let open B in
    ite (is_finite fsort x && is_finite fsort y)
      (fmul_finite fsort rm x y)
      (fmul_special fsort x y)

  let mask_bit sort i =
    let uno = B.one sort in
    let shf = B.of_int sort i in
    B.(uno lsl shf)

  (* pre: nominator > denominator
     the msb of result can be 1 only (and only) in rare
     case of applying rounding to 01111..11. And we need to
     to check msb manually, since round function itself
     will not help us.  *)
  let long_division prec rm sign nomin denom =
    let open B in
    nomin >>-> fun nomin sort ->
    let rec loop i bits nomin =
      if Caml.(i < 0) then
        List.fold bits ~f:(lor) ~init:(zero sort) >=> fun coef ->
        match_ [
            (nomin > denom) --> Lost.alot sort;
            (nomin = denom) --> Lost.half sort;
            (nomin = zero sort) --> Lost.zero sort;
         ] ~default:(Lost.afew sort) >=> fun loss ->
        round rm sign coef loss (Lost.bits sort) @@ fun coef _ ->
        coef
      else
        ite (nomin > denom) (mask_bit sort i) (zero sort) >=> fun bit ->
        ite (nomin > denom) (nomin - denom) nomin >=> fun next_nomin ->
        next_nomin lsl one sort >=> fun next_nomin ->
        bind next_nomin (fun next_nomin ->
          loop Caml.(i - 1) (bit :: bits) next_nomin) in
    loop Caml.(prec - 1) [] !!nomin

  let fdiv_raw fsort rm xsign xexpn xcoef ysign yexpn ycoef f =
    let open B in
    let norm_nominator exps sigs nomin denom f =
      ite (nomin < denom) (nomin lsl one sigs) nomin >=> fun nomin' ->
      ite (nomin < denom) (one exps) (zero exps) >=> fun dexpn ->
      f nomin' dexpn in
    let prec = precision fsort in
    let exps,sigs = floats fsort in
    xor xsign ysign >=> fun sign ->
    normalize_coef xcoef @@ fun nomin dx ->
    normalize_coef ycoef @@ fun denom dy ->
    dy - dx >=> fun de ->
    extend nomin ~addend:1 @@ fun nomin sigs' ->
    extend denom ~addend:1 @@ fun denom _ ->
    norm_nominator exps sigs' nomin denom @@ fun nomin dexpn' ->
    long_division prec rm sign nomin denom >=> fun coef ->
    msb coef >=> fun coef_overflow ->
    ite coef_overflow (coef lsr one sigs) coef >=> fun coef ->
    ite coef_overflow (one exps) (zero exps) >=> fun from_rnd ->
    unsigned sigs coef >=> fun coef ->
    of_int exps (bias fsort) >=> fun bias ->
    from_rnd - dexpn' + unsigned exps de >=> fun dexpn ->
    xexpn - yexpn >=> fun expn ->
    ((xexpn < yexpn) && (yexpn - xexpn > dexpn + bias - min_exponent exps)) >=> fun underflowed ->
    ((xexpn > yexpn) && (expn > max_exponent exps - (dexpn + bias))) >=> fun is_overflow ->
    ((xexpn < yexpn) && (yexpn - xexpn > of_int exps prec + dexpn + bias - min_exponent exps)) >=> fun is_underflow ->
    expn + dexpn + bias  >=> fun expn ->
    ite underflowed (abs expn + min_exponent exps) expn >=> fun shift ->
    ite underflowed (extract_last coef shift) (zero sigs) >=> fun loss ->
    ite underflowed (coef lsr shift) coef >=> fun coef ->
    round rm sign coef loss shift @@ fun coef _ ->
    ite underflowed (min_exponent exps) expn >=> fun expn ->

    ite is_underflow (zero exps) expn >=> fun expn ->
    ite is_overflow  (ones exps) expn >=> fun expn ->
    ite (is_overflow || is_underflow) (zero sigs) coef >=> fun coef ->
    norm expn coef @@ fun expn coef -> f sign expn coef

  let fdiv_special fsort x y =
    let open B in
    with_special fsort x @@ fun ~is_inf:xinf ~is_snan:xsnan ~is_qnan:xqnan ->
    with_special fsort y @@ fun ~is_inf:yinf ~is_snan:ysnan ~is_qnan:yqnan ->
    fsign x >=> fun xsign ->
    fsign y >=> fun ysign ->
    (xinf && yinf) >=> fun is_inf ->
    inv (xinf || xsnan || xsnan) >=> fun is_finx ->
    inv (yinf || ysnan || ysnan) >=> fun is_finy ->
    xor xsign ysign >=> fun sign ->
    match_ [
      (is_zero x && is_zero y) --> qnan fsort;
      (is_zero x && is_finy) --> fzero fsort sign;
      (is_zero y && is_finx) --> inf fsort sign;
      (xinf && yinf) --> qnan fsort;
      is_inf --> with_sign (xor xsign ysign) x;
      (xsnan || xqnan) --> transform_to_quite fsort x
    ] ~default:(transform_to_quite fsort y)

  let fdiv_finite fsort rm x y =
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun ysign yexpn ycoef ->
    fdiv_raw fsort rm xsign xexpn xcoef ysign yexpn ycoef @@
     fun sign expn coef -> pack fsort sign expn coef

  let fdiv fsort rm x y =
    let open B in
    ite (is_finite_nonzero fsort x && is_finite_nonzero fsort y)
      (fdiv_finite fsort rm x y)
      (fdiv_special fsort x y)

  let fsqrt fsort rm x =
    fadd_finite fsort rm (fone fsort) (fone fsort) >=> fun two ->
    fdiv_finite fsort rm x two >=> fun init ->
    let max = precision fsort in
    let rec run x0 n =
      fdiv_finite fsort rm x x0 >=> fun a1 ->
      fadd_finite fsort rm x0 a1 >=> fun a2 ->
      fdiv_finite fsort rm a2 two >=> fun x' ->
      if n > max then x0
      else run x' (n + 1) in
    run init 0

  let gen_cast_float fsort rmode sign bitv =
    let open IEEE754 in
    let open B in
    let {p;bias} = Sort.spec fsort in
    let exps = exps fsort in
    let sigs = Bits.define p in
    bitv >>-> fun bitv inps ->
    of_int exps Caml.(bias + p - 1) >=> fun expn ->
    of_int sigs p >=> fun prec ->
    clz !!bitv >=> fun clz ->
    unsigned sigs clz >=> fun clz ->
    of_int sigs (Bits.size inps) - clz >=> fun msbn ->
    ite (msbn > prec)
      (msbn - prec >=> fun de ->
       msbn - one sigs >=> fun hi ->
       hi - prec + one sigs >=> fun lo ->
       extract sigs hi lo !!bitv >=> fun coef ->
       extract sigs (pred lo) (zero sigs) !!bitv >=> fun loss ->
       round rne sign coef loss lo @@ fun coef rnd_overflow ->
       ite rnd_overflow (not (ones sigs lsr one sigs)) coef >=> fun coef ->
       ite rnd_overflow (succ expn) expn >=> fun expn ->
       expn + unsigned exps de >=> fun expn ->
       norm expn coef @@ fun expn coef ->
       pack fsort sign expn coef)
    (unsigned sigs !!bitv >=> fun coef ->
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
    expn - of_int exps bias + one exps >=> fun bits ->
    of_int sigs p - unsigned sigs bits >=> fun bits ->
    coef lsr unsigned sigs bits >=> fun coef ->
    unsigned outs coef >=> fun coef ->
    ite sign (neg coef) coef

end