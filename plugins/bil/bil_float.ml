open Core_kernel
open Bap.Std

open Bap_knowledge
open Bap_core_theory

type rounding =
  | Nearest_even  (** round to nearest, ties to even *)
  | Nearest_away  (** round to nearest, ties to away *)
  | Towards_zero  (** round toward zero              *)
  | Positive_inf  (** round toward positive infinity *)
  | Negative_inf  (** round toward negative infinity *)
[@@deriving sexp]

type 'a t = 'a knowledge

type ('b, 'e, 't, 's) binop =
  ((('b,'e,'t) IEEE754.t,'s) format float sort ->
   rmode value t -> 's bitv value t -> 's bitv value t -> 's bitv value t)


module Rounding = struct
  open Knowledge.Syntax

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
    | None -> !! None
    | Some r -> !! (Some r)
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

  let sort x = x >>= fun x -> !! (Value.sort x)
  let size x = sort x >>= fun s -> !! (Bits.size s)
  let sema x = x >>= fun x -> !! (Value.semantics x)
  let empty dom sort x = !! (Value.put dom (Value.empty sort) x)

  let bind a body =
    a >>= fun a ->
    sort !!a >>= fun s ->
    Var.Generator.fresh s >>= fun v ->
    B.let_ v !!a (body (B.var v))

  let (>=>) = bind

  let floats fsort =
    let open IEEE754 in
    let exps = Sort.exps fsort in
    let spec = Sort.spec fsort in
    let sigs = Bits.define spec.p in
    exps, sigs

  let fsign = B.msb

  let exponent fsort bitv =
    let open IEEE754 in
    let bits = Sort.bits fsort in
    let exps = Sort.exps fsort in
    let spec = Sort.spec fsort in
    let shift = B.of_int bits spec.t in
    B.low exps B.(bitv lsr shift)

  let significand fsort expn bitv =
    let open IEEE754 in
    let spec = Sort.spec fsort in
    let sigs = Bits.define spec.t in
    let coef = B.low sigs bitv in
    if spec.t = spec.p then coef
    else
      let sigs' = Bits.define spec.p in
      let bit = Bits.define 1 in
      let leading_bit = B.(ite (is_zero expn) (zero bit) (one bit)) in
      B.append sigs' leading_bit coef

  let pack fsort sign expn coef =
    let open B in
    let open IEEE754 in
    let bits = Sort.bits fsort in
    let bit = Bits.define 1 in
    let spec = Sort.spec fsort in
    let coef =
      if Caml.(spec.p = spec.t) then coef
      else
        let sigs = Bits.define spec.t in
        B.low sigs coef in
    let bits_1 = Bits.define Caml.(Bits.size bits - 1) in
    let sign = ite sign (B.one bit) (B.zero bit) in
    B.append bits sign (B.append bits_1 expn coef)

  let unpack fsort x f =
    exponent fsort x >=> fun expn ->
    significand fsort expn x >=> fun coef ->
    f (fsign x) expn coef

  let is_inf fsort x =
    unpack fsort x @@ fun _ expn coef ->
    B.(and_ (is_zero coef) (is_all_ones expn))

  let is_nan fsort x =
    unpack fsort x @@ fun _ expn coef ->
    B.(and_ (non_zero coef) (is_all_ones expn))

  let is_special fsort x = B.or_ (is_nan fsort x) (is_inf fsort x)
  let is_finite fsort x = B.inv (is_special fsort x)

  let precision fsort = let {IEEE754.p} = IEEE754.Sort.spec fsort in p
  let bias fsort = let {IEEE754.bias} = IEEE754.Sort.spec fsort in bias

  let match_ cases ~default =
    let cases = List.rev cases in
    List.fold cases ~init:default
      ~f:(fun fin (cond, ok) -> B.ite cond ok fin)

  let (-->) x y = x,y

  let extract_last x n =
    let open B in
    sort x >>= fun sort ->
    x >>= fun v ->
    let mask = not (ones sort lsl n)  in
    x land mask

  let rshift_coef coef n =
    B.(coef lsr n), extract_last coef n

  let half_of_loss loss lost_bits =
    let open B in
    loss >>= fun vloss ->
    let vsort = Value.sort vloss in
    let x = one vsort in
    let n = pred lost_bits in
    x lsl n

  (* TODO: consider to add expn here and if all ones - adjust it  *)
  let round rm sign coef loss lost_bits =
    let open Rmode in
    let open B in
    Rounding.get rm >>= fun rm ->
    sort coef >>= fun sigs ->
    let loss = extract_last loss lost_bits in
    let half = half_of_loss loss lost_bits in
    let is_needed = match rm with
      | Some Positive_inf -> B.inv sign
      | Some Negative_inf -> sign
      | Some Nearest_away -> loss >= half
      | Some Nearest_even ->
         or_ (loss > half) (and_ (loss = half) (lsb coef))
      | _ -> b0 in
    let is_needed = and_ (non_zero lost_bits) is_needed in
    let all_ones = not (zero sigs) in
    ite (and_ (coef <> all_ones) is_needed) (succ coef) coef

  (* maximum possible exponent that fits in [n - 1] bits. (one for sign) *)
  let max_exponent' n = int_of_float (2.0 ** (float_of_int n )) - 2
  let min_exponent' n = 0
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
    sort x >>= fun sort ->
    size x >>= fun size ->
    let pow, num = nearest_pow2 size in
    let sort' = Bits.define num in
    let shifts = List.init pow ~f:(fun p -> num / (2 lsl p)) in
    let shifts,_ =
      List.fold shifts ~init:([],0)
        ~f:(fun (acc,prev) curr ->
            let total = curr + prev in
            (total, curr) :: acc, total) in
    let shifts = List.rev shifts in
    let x' = B.unsigned sort' x in
    let rec loop lets x = function
      | [] -> List.fold lets ~f:B.(+) ~init:(B.zero sort)
      | (total, shift) :: shifts ->
        let shift = B.of_int sort shift in
        let total = B.of_int sort total in
        let mask = B.(ones sort' lsl total) in
        let nextx = B.(ite (is_zero (x land mask)) (x lsl shift) x) in
        let nextn = B.(ite (is_zero (x land mask)) shift (B.zero sort)) in
        bind nextx @@ fun nextx ->
        bind nextn @@ fun nextn ->
        loop (nextn :: lets) nextx shifts in
    let n = loop [] x' shifts in
    let dif = B.of_int sort (num - size) in
    B.ite (B.is_zero x) (B.of_int sort size) B.(n - dif)

  let possible_lshift expn coef =
    let open B in
    sort expn >>= fun exps ->
    sort coef >>= fun sigs ->
    let min_expn = min_exponent exps in
    clz coef >=> fun clz ->
    min clz (unsigned sigs (abs (expn - min_expn)))

  (* TODO: consider test coef for zero to prevent expn change *)
  let safe_align_left expn coef =
    let open B in
    sort expn >>= fun exps ->
    sort coef >>= fun sigs ->
    let shift = possible_lshift expn coef in
    let dexpn = unsigned exps shift in
    let coef = coef lsl shift in
    let expn = expn - dexpn in
    !! (expn,coef)

  let msbn x =
    let open B in
    sort x >>= fun sort ->
    size x >>= fun prec ->
    bind (clz x) (fun clz ->
        of_int sort prec - clz - one sort)

  (* min exponent without bit loss or exponent overflow,
     fraction shifted as left as possible, i.e. it occupies
     more significant bits *)
  let minimize_exponent = safe_align_left
  let norm = safe_align_left

   (* returns result sign *)
  let xor_sign s s' = B.(and_ (or_ s s') (inv (and_ s s')))

  (** [combine_loss more_significant less_significant length_of_less ] *)
  let combine_loss more less length_of_less =
    let more = B.(more lsl length_of_less) in
    B.(more lor less)

  let fadd fsort rm x y =
    let open B in
    let exps,sigs = floats fsort in
    unpack fsort x @@ fun sign xexpn xcoef ->
    unpack fsort y @@ fun _ yexpn ycoef ->
    sort xexpn >>= fun xes ->
    sort xcoef >>= fun xec ->
    let lost_bits = abs (xexpn - yexpn) in
    ite (xexpn > yexpn) xcoef (xcoef lsr lost_bits) >=> fun xcoef ->
    ite (yexpn > xexpn) ycoef (ycoef lsr lost_bits) >=> fun ycoef ->
    xcoef + ycoef >=> fun coef ->
    max xexpn yexpn >=> fun expn ->
    ite (coef >= xcoef) expn (succ expn) >=> fun expn ->
    match_ [
          (xexpn = yexpn) --> zero sigs;
          (xexpn > yexpn) --> extract_last ycoef lost_bits;
      ] ~default:(extract_last xcoef lost_bits) >=> fun loss ->
    extract_last coef (one sigs) >=> fun loss' ->
    coef >= xcoef >=> fun no_overflow ->
    ite no_overflow lost_bits (succ lost_bits) >=> fun lost_bits ->
    ite no_overflow loss (combine_loss loss' loss lost_bits) >=> fun loss ->
    ite no_overflow coef (coef lsr one sigs) >=> fun coef ->
    one sigs lsl (of_int sigs Caml.(Bits.size sigs - 1)) >=> fun leading_one ->
    ite no_overflow coef (coef lor leading_one) >=> fun coef ->
    round rm sign coef loss lost_bits >=> fun coef ->
    norm expn coef >>= fun (expn,coef) ->
    pack fsort sign expn coef

  let extend bitv ~addend  =
    sort bitv >>= fun sort ->
    let sort' = Bits.define (Bits.size sort + addend) in
    B.unsigned sort' bitv

  let invert_loss loss lost_bits =
    let open B in
    sort loss >>= fun sort ->
    let half = half_of_loss loss lost_bits in
    let inverted =
      let mask = not (ones sort lsl lost_bits) in
      mask land (not loss) in
    match_ [
        is_zero lost_bits --> zero sort;
        is_zero loss --> loss;
        (loss = half) --> loss;
      ] ~default:inverted

  let match_cmp x y ~on_eql ~on_gt ~on_lt =
    let open B in
    match_ [
      (x =  y) --> on_eql;
      (x >$ y) --> on_gt;
    ] ~default:on_lt

  let fsub fsort rm x y =
    let open B in
    let exps,sigs = floats fsort in
    let min_expn = min_exponent exps in
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun _     yexpn ycoef ->
    extend xcoef ~addend:1 >=> fun xcoef ->
    extend ycoef ~addend:1 >=> fun ycoef ->
    sort xcoef >>= fun sigs' ->
    abs (xexpn - yexpn) >=> fun diff ->
    ite (is_zero diff) diff (diff - one exps) >=> fun lost_bits ->
    match_ [
        (xexpn > yexpn) --> extract_last ycoef lost_bits;
        (xexpn < yexpn) --> extract_last xcoef lost_bits;
      ] ~default:(zero sigs') >=> fun loss ->
    invert_loss loss lost_bits >=> fun loss ->
    or_ (xexpn < yexpn) (and_ (xexpn = yexpn) (xcoef < ycoef)) >=> fun swap ->
    ite swap (inv xsign) xsign >=> fun sign ->
    match_ [
        (xexpn > yexpn) --> xcoef lsl one sigs';
        (xexpn < yexpn) --> xcoef lsr lost_bits;
      ] ~default:xcoef >=> fun xcoef ->
    match_ [
        (yexpn > xexpn) --> ycoef lsl one sigs';
        (yexpn < xexpn) --> ycoef lsr lost_bits;
      ] ~default:ycoef >=> fun ycoef ->
    ite (is_zero loss) (zero sigs') (one sigs') >=> fun borrow ->
    ite swap (ycoef - xcoef - borrow) (xcoef - ycoef - borrow) >=> fun coef ->
    msb coef >=> fun msbc ->
    max xexpn yexpn >=> fun expn ->
    ite (xexpn = yexpn) expn (expn - one exps) >=> fun expn ->
    ite (is_zero coef) min_expn (ite msbc (succ expn) expn) >=> fun expn ->
    ite msbc (extract_last coef (one sigs')) (zero sigs') >=> fun loss' ->
    ite msbc (combine_loss loss' loss lost_bits) loss >=> fun loss ->
    ite msbc (succ lost_bits) lost_bits >=> fun lost_bits ->
    ite msbc (coef lsr one sigs') coef >=> fun coef ->
    unsigned sigs coef >=> fun coef ->
    round rm sign coef loss lost_bits >=> fun coef ->
    norm expn coef >>= fun (expn,coef) ->
    pack fsort sign expn coef

let add_or_sub ~is_sub fsort rm x y =
    let ( lxor ) = xor_sign in
    let s1 = is_sub in
    let s2 = fsign x in
    let s3 = fsign y in
    let is_sub = s1 lxor (s2 lxor s3) in
    B.ite is_sub (fsub fsort rm x y) (fadd fsort rm x y)

  let fsub fsort rm x y = add_or_sub ~is_sub:B.b1 fsort rm x y
  let fadd fsort rm x y = add_or_sub ~is_sub:B.b0 fsort rm x y

  let double bitv =
    sort bitv >>= fun sort ->
    extend bitv ~addend:(Bits.size sort)

  let fmul fsort rm x y =
    let open B in
    let exps,sigs = floats fsort in
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun ysign yexpn ycoef ->
    double xexpn >=> fun xexpn ->
    double yexpn >=> fun yexpn ->
    double xcoef >=> fun xcoef ->
    double ycoef >=> fun ycoef ->
    sort xexpn >>= fun exps' ->
    sort xcoef >>= fun sigs' ->
    xexpn + yexpn - of_int exps' (bias fsort) >=> fun expn ->
    xcoef * ycoef >=> fun coef ->
    xor_sign xsign ysign >=> fun sign ->
    clz coef >=> fun clz ->
    of_int sigs' (precision fsort) >=> fun prec ->
    ite (clz >= prec) (zero sigs') (prec - clz) >=> fun shift ->
    extract_last coef shift >=> fun loss ->
    coef lsr shift >=> fun coef ->
    round rm sign coef loss shift >=> fun coef ->
    unsigned sigs coef >=> fun coef ->
    unsigned exps expn >=> fun expn ->
    min_exponent exps >=> fun min ->
    ite (is_zero coef) min expn >=> fun expn ->
    norm expn coef >>= fun (expn,coef) ->
    pack fsort sign expn coef

  let mask_bit sort i =
    let uno = B.one sort in
    let shf = B.of_int sort i in
    B.(uno lsl shf)

  let long_division prec rm sign nomin denom =
    let open B in
    sort nomin >>= fun sort ->
    let rec loop i bits nomin =
      if Caml.(i < 0) then
        List.fold bits ~f:(lor) ~init:(zero sort) >=> fun coef ->
        let loss = match_ [
            (nomin > denom) --> Lost.alot sort;
            (nomin = denom) --> Lost.half sort;
            (nomin = zero sort) --> Lost.zero sort;
         ] ~default:(Lost.afew sort) in
        round rm sign coef loss (Lost.bits sort)
      else
        let next_nomin = ite (nomin > denom) (nomin - denom) nomin in
        let bit = ite (nomin > denom) (mask_bit sort i) (zero sort) in
        let bits = bit :: bits in
        bind next_nomin (fun next_nomin ->
          loop Caml.(i - 1) bits (next_nomin lsl one sort)) in
    loop Caml.(prec - 1) [] nomin

  let fdiv fsort rm x y =
    let open B in
    let prec = precision fsort in
    let exps,sigs = floats fsort in
    unpack fsort x @@ fun xsign xexpn xcoef ->
    unpack fsort y @@ fun ysign yexpn ycoef ->
    extend xcoef ~addend:1 >=> fun xcoef ->
    extend ycoef ~addend:1 >=> fun ycoef ->
    sort xcoef >>= fun sigs' ->
    xor_sign xsign ysign >=> fun sign ->
    ite (xcoef < ycoef) (xcoef lsl one sigs') xcoef >=> fun nomin ->
    long_division prec rm sign nomin ycoef >=> fun coef ->
    unsigned sigs coef >=> fun coef ->
    ite (xcoef < ycoef) (one exps) (zero exps) >=> fun dexpn ->
    xexpn - yexpn - dexpn + of_int exps (bias fsort) >=> fun expn ->
    norm expn coef >>= fun (expn,coef) ->
    pack fsort sign expn coef

end
