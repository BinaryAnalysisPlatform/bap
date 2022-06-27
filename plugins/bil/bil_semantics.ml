open Core_kernel[@@warning "-D"]
open Bap.Std
open Bap_knowledge
open Bap_core_theory

[@@@warning "-40"]

let exp = Exp.slot
let stmt = Bil.slot

let values = exp
let effects = stmt
let bool = Theory.Bool.t
let bits = Theory.Bitv.define
let size = Theory.Bitv.size
let sort = Theory.Value.sort
let resort s x = KB.Value.refine x s

module Basic : Theory.Basic = struct
  open Knowledge.Syntax

  module Base = struct

    let ret = Knowledge.return

    let value x = KB.Value.get exp x

    let v s e = KB.Value.put exp (Theory.Value.empty s) e

    let (%:) e s = v s e


    let exp s x = ret @@ x %: s
    let bit x = ret @@ x %: Theory.Bool.t
    let mem s v = ret @@ v %: s
    let vec s v = ret @@ v %: s

    let unk s' =
      let s = Theory.Value.Sort.forget s' in
      ret @@ match Theory.Bool.refine s with
      | Some _ -> Bil.unknown "bits" bool_t %: s'
      | None -> match Theory.Bitv.refine s with
        | Some b ->
          Bil.unknown "bits" (Type.imm (Theory.Bitv.size b)) %: s'
        | None -> Bil.unknown "unk" Type.Unk %: s'

    let empty = Theory.Effect.empty Theory.Effect.Sort.bot
    let eff d = ret @@ KB.Value.put stmt empty d
    let data s = eff s
    let ctrl s = eff s
    let bool_exp : _ -> Bil.exp = value
    let bitv_exp : _ -> Bil.exp = value

    let var r =
      let v = Var.reify r in
      exp (Theory.Var.sort r) (Var v)

    let b0 = bit Bil.(int Word.b0)
    let b1 = bit Bil.(int Word.b1)

    let int s w = vec s @@ Bil.(int @@ Word.create w (size s))

    let effect x = KB.Value.get effects x

    let (>>->) v f = v >>= fun v -> f (sort v) (value v)

    let lift1 mk s f v = v >>-> fun sort x -> mk (s sort) (f x)

    let lift2 mk s f x y =
      x >>-> fun sx x ->
      y >>-> fun sy y ->
      mk (s sx sy) (f x y)

    let lift3 mk s f x y z =
      x >>-> fun sx x ->
      y >>-> fun sy y ->
      z >>-> fun sz z ->
      mk (s sx sy sz) (f x y z)

    type 'a sort = 'a Theory.Value.sort
    type bit = Theory.Bool.t

    (* typing rules *)
    let t_lo1 : 'a sort -> bit sort = fun _ -> bool
    let t_lo2 : 'a sort -> 'a sort -> bit sort = fun _ _ -> bool
    let t_uop : 'a sort -> 'a sort = fun x -> x
    let t_aop : 'a sort -> 'a sort -> 'a sort = fun x _ -> x
    let t_sop : 'a sort -> 'b sort -> 'a sort = fun x _ -> x

    (* operators *)
    let lo1 x = lift1 (fun _ x -> bit x) t_lo1 x
    let lo2 x y = lift2 (fun _ x -> bit x) t_lo2 x y
    let uop x = lift1 vec t_uop x
    let aop x y = lift2 vec t_aop x y
    let sop x y = lift2 vec t_sop x y

    let or_ x = lo2 Bil.(lor) x
    let and_ x = lo2 Bil.(land) x

    let inv x = lo1 Bil.(lnot) x
    let msb x = lo1 Bil.(cast high 1) x
    let lsb x = lo1 Bil.(cast low 1) x
    let neg x = uop Bil.(unop neg) x
    let not   x = uop Bil.(lnot) x
    let add x y = aop Bil.(+) x y
    let sub x y = aop Bil.(-) x y
    let mul x y = aop Bil.( * ) x y
    let div x y = aop  Bil.(/) x y
    let sdiv x y = aop Bil.(/$) x y
    let modulo x y = aop Bil.(mod) x y
    let smodulo x y = aop Bil.(%$) x y
    let logand x y = aop Bil.(land) x y
    let logor x y = aop Bil.(lor) x y
    let logxor x y = aop Bil.(lxor) x y
    let ule x y = lo2 Bil.(<=) x y
    let sle x y = lo2 Bil.(<=$) x y
    let eq x y = lo2 Bil.(=) x y
    let neq x y = lo2 Bil.(<>) x y
    let slt x y = lo2 Bil.(<$) x y
    let ult x y = lo2 Bil.(<) x y

    let sgt x y = slt y x
    let ugt x y = ult y x
    let sge x y = sle y x
    let uge x y = ule y x

    let small s x = Bil.Int (Word.of_int ~width:(size s) x)

    let mk_zero s = Bil.Int (Word.zero (size s))

    let is_zero x =
      x >>= fun x ->
      let s = sort x in
      bit @@ Bil.(bitv_exp x = mk_zero s)

    let non_zero x =
      x >>= fun x ->
      let s = sort x in
      bit @@ Bil.(bitv_exp x <> mk_zero s)

    let shiftr b x y =
      b >>-> fun _s b ->
      x >>-> fun xs x ->
      y >>-> fun _ y ->
      vec xs @@
      if Exp.equal b (Bil.int Word.b0) then Bil.(x lsr y)
      else
        let ones = Word.ones (size xs) in
        let mask = Bil.(lnot (int ones lsr y)) in
        Bil.(ite b ((x lsr y) lor mask) (x lsr y))

    let shiftl b x y =
      b >>-> fun _s b ->
      x >>-> fun xs x ->
      y >>-> fun _ y ->
      vec xs @@
      if Exp.equal b (Bil.int Word.b0) then Bil.(x lsl y)
      else
        let ones = Word.ones (size xs) in
        let shifted = Bil.(int ones lsl y)  in
        let mask = Bil.(lnot shifted) in
        let lhs = Bil.(x lsl y) in
        let yes = Bil.(lhs lor mask) in
        let nay = Bil.(x lsl y)  in
        Bil.(ite b yes nay)

    let app_bop lift op x y = lift (Bil.binop op) x y
    let arshift x y = app_bop sop Bil.arshift x y
    let rshift x y = app_bop sop Bil.rshift x y
    let lshift x y = app_bop sop Bil.lshift x y

    let ite cnd yes nay =
      cnd >>| bool_exp >>= fun cnd ->
      yes >>-> fun s yes ->
      nay >>-> fun _ nay ->
      match yes,nay with
      | Bil.Int w, Bil.Int w' when Word.bitwidth w = 1 ->
        if Word.is_one w && Word.is_zero w'
        then exp s cnd
        else if Word.is_zero w && Word.is_one w'
        then exp s (Bil.lnot cnd)
        else exp s (Bil.ite cnd yes nay)
      | _ ->
        exp s (Bil.ite cnd yes nay)

    let (>>:=) v f = v >>= fun v -> f (effect v)

    let branch cnd yes nay =
      cnd >>= fun cnd ->
      yes >>= fun yes ->
      nay >>:= fun nay ->
      eff Bil.[if_ (bool_exp cnd) (effect yes) nay]

    let make_cast s t x =
      x >>-> fun _ x -> vec s @@ Bil.cast t (size s) x

    let high s = make_cast s Bil.high
    let low s = make_cast s Bil.low
    let signed s = make_cast s Bil.signed
    let unsigned s = make_cast s Bil.unsigned

    let mask_high res n =
      let width = size res in
      let n = Word.of_int ~width n in
      let w = Word.(lnot (ones width lsr n)) in
      int res (Word.to_bitvec w)

    let cast res b x =
      b >>= fun b ->
      x >>= fun x ->
      let sort = sort x in
      let src = bitv_exp x in
      let fill = bool_exp b in
      let diff = size res - size sort in
      let cast kind = vec res @@ Bil.cast kind (size res) src in
      match compare diff 0,fill with
      | 0,_ -> vec res src
      | 1, Bil.Int b ->
        if Word.(b = b0) then cast Bil.unsigned
        else ite (msb !!x)
            (cast Bil.signed)
            (logor (cast Bil.unsigned) (mask_high res diff))
      | 1, _ ->
        ite !!b
          (logor (cast Bil.unsigned) (mask_high res diff))
          (cast Bil.unsigned)
      | _ -> vec res Bil.(cast low (size res) src)

    let append s ex ey =
      ex >>= fun ex ->
      ey >>= fun ey ->
      let sx = sort ex and sy = sort ey in
      let x = bitv_exp ex and y = bitv_exp ey in
      match compare (size sx + size sy) (size s) with
      | 0 -> vec s (Bil.concat x y)
      | 1 ->
        let extra = size s - size sx - size sy in
        vec s @@ Bil.(cast unsigned extra (concat x y))
      | _ ->
        if size s < size sx
        then vec s Bil.(cast low (size s) x)
        else vec s Bil.(cast low (size s) (concat x y))

    let rec uncat acc : Bil.exp -> Bil.exp list = function
      | Concat ((Concat (x,y)), z) -> uncat (y::z::acc) x
      | Concat (x,y) -> x::y::acc
      | x -> x::acc

    let concat s vs = match vs with
      | [] -> unk s
      | _ ->
        Knowledge.List.all vs >>= fun vs ->
        let sz = List.fold ~init:0 vs ~f:(fun sz x ->
            sz + size (sort x)) in
        let x = List.reduce_exn ~f:Bil.concat @@
          List.map vs ~f:bitv_exp in
        cast s b0 (vec (bits sz) x)

    let load mem key =
      mem >>-> fun sort mem ->
      key >>-> fun _ key ->
      let vals = Theory.Mem.vals sort in
      match Size.of_int_opt (size vals) with
      | Some sz ->
        exp vals Bil.(load mem key BigEndian sz)
      | None -> unk vals

    let store m k d =
      m >>-> fun ms m ->
      k >>-> fun _ k ->
      d >>-> fun ds d ->
      match Size.of_int_opt (size ds) with
      | Some rs ->
        exp ms Bil.(store ~mem:m ~addr:k d BigEndian rs)
      | _ -> unk ms

    let perform s = ret (Theory.Effect.empty s)
    let pass = data []
    let skip = ctrl []

    let seq x y =
      x >>= fun x ->
      y >>= fun y ->
      eff (effect x @ effect y)

    let blk lbl x y =
      x >>:= fun x ->
      y >>:= fun y ->
      if KB.Object.is_null lbl
      then eff (x @ y)
      else
        let dst = Tid.to_string lbl in
        eff Bil.(encode label dst :: x @ y)

    let recursive_simpl = Exp.simpl ~ignore:Bap.Std.Eff.[load;store;read]

    let let_ var rhs body =
      let v = Var.reify var in
      rhs >>-> fun _ rhs ->
      body >>-> fun bs body ->
      match rhs with
      | (Int _) as rhs ->
        exp bs @@ recursive_simpl @@ Let (v,rhs,body)
      | _ -> exp bs @@ Bil.let_ v rhs body

    let set lhs rhs =
      rhs >>-> fun _ rhs ->
      data [Bil.(Var.reify lhs := rhs)]

    let repeat cnd body =
      cnd >>= fun cnd ->
      body >>:= fun body ->
      data [Bil.while_ (bool_exp cnd) body]

    let jmp dst =
      dst >>= fun dst -> ctrl [Bil.jmp (bitv_exp dst)]

    let goto lbl = ctrl [
        Bil.(encode goto (Tid.to_string lbl))
      ]


  end

  include Theory.Basic.Make(Base)
  include Base

  let loadw rs cnd mem key =
    match Size.of_int_opt (size rs) with
    | None -> loadw rs cnd mem key
    | Some sz ->
      cnd >>= fun cnd ->
      key >>= fun key ->
      mem >>-> fun _ mem ->
      let dir = bool_exp cnd and key = bitv_exp key in
      let bel = vec rs @@ Bil.load mem key BigEndian sz
      and lel = vec rs @@ Bil.load mem key LittleEndian sz in
      match dir with
      | Int dir -> if Word.(dir = b1) then bel else lel
      | _ -> ite !!cnd bel lel

  let storew cnd mem key elt =
    elt >>-> fun es e ->
    match Size.of_int_opt (size es) with
    | None -> storew cnd mem key elt
    | Some sz ->
      cnd >>| value >>= fun dir ->
      key >>| value >>= fun key ->
      mem >>-> fun sort mem ->
      let bes = exp sort @@ Bil.store mem key e BigEndian sz
      and les = exp sort @@ Bil.store mem key e LittleEndian sz in
      match dir with
      | Int dir -> if Word.(dir = b1) then bes else les
      | _ -> ite (bit dir) bes les

  let extract s hi lo x =
    hi >>= fun hi ->
    lo >>= fun lo ->
    x  >>= fun e ->
    match value hi,value lo with
    | Int h, Int l ->
      let h = Word.to_int_exn h
      and l = Word.to_int_exn l in
      if h - l + 1 = size s
      then vec s @@ Bil.(extract ~hi:h ~lo:l (value e))
      else extract s !!hi !!lo !!e
    | _ -> extract s !!hi !!lo !!e

  let goto lbl =
    KB.collect Theory.Label.addr lbl >>= fun dst ->
    match dst with
    | Some addr ->
      Theory.Label.target lbl >>= fun target ->
      let size = Theory.Target.code_addr_size target in
      let dst = Word.create addr size in
      ctrl Bil.[Jmp (Int dst)]
    | _ -> KB.collect Theory.Label.ivec lbl >>= function
      | Some ivec -> ctrl Bil.[CpuExn ivec]
      | None -> KB.collect Theory.Label.name lbl >>= fun name ->
        ctrl @@ match name with
        | Some name -> [Bil.(encode call name)]
        | None -> [Bil.(encode goto (Tid.to_string lbl))]


  let fbits x =
    x >>= fun x ->
    unk @@ Theory.Float.bits (sort x)
end

module Rmode = struct
  open KB.Syntax
  let s = bits 8

  let rmode n =
    let+ n = Basic.int s (Bitvec.M8.int n) in
    resort Theory.Rmode.t n

  let rne = rmode 0
  let rna = rmode 1
  let rtp = rmode 2
  let rtn = rmode 3
  let rtz = rmode 4

  let requal x y =
    let* x = x >>| resort s in
    let* y = y >>| resort s in
    Basic.eq !!x !!y

end

module CT : Theory.Core = struct
  include Theory.Empty
  include Basic
  include Rmode
end

module Float = Bil_float.Make(CT)

module Emulator = struct
  open Knowledge.Syntax
  type 'a t = 'a knowledge

  let supported = Theory.IEEE754.[
      binary16;
      binary32;
      binary64;
      binary80;
      binary128;
    ]

  let ieee754_of_sort s =
    List.find supported ~f:(fun p ->
        Theory.Value.Sort.same s (Theory.IEEE754.Sort.define p))

  let fbits x =
    x >>| fun x -> resort (Theory.Float.bits (sort x)) x

  let float s x =
    x >>| fun x -> resort s x


  let with_fsort ~unk_s s f =
    match ieee754_of_sort s with
    | None -> CT.unk unk_s
    | Some ({Theory.IEEE754.k} as p) ->
      f (bits k) (Theory.IEEE754.Sort.define p)

  let fop : type f.
    (_ -> _ -> _ Theory.bitv -> _ Theory.bitv -> _ Theory.bitv) ->
    _ -> f Theory.float -> f Theory.float -> f Theory.float =
    fun op rm x y ->
    x >>= fun x ->
    y >>= fun y ->
    let xs = sort x in
    with_fsort xs ~unk_s:xs @@ fun bs s ->
    let x = resort bs x and y = resort bs y in
    float xs (op s rm !!x !!y)

  let fadd rm = fop Float.fadd rm
  let fsub rm = fop Float.fsub rm
  let fmul rm = fop Float.fmul rm
  let fdiv rm = fop Float.fdiv rm

  let fuop : type f.
    _ ->
    _ -> f Theory.float -> f Theory.float =
    fun op rm x ->
    x >>= fun x ->
    let xs = sort x in
    with_fsort ~unk_s:xs xs @@ fun bs s ->
    let x = resort bs x in
    float xs (op s rm !!x)

  let fsqrt rm x = fuop Float.fsqrt rm x

  open CT

  let small s x =
    let m = Bitvec.modulus (size s) in
    int s Bitvec.(int x mod m)

  let classify {Theory.IEEE754.w; t} v ~fin ~inf ~nan =
    let ws = bits w and fs = bits t in
    let expn = extract ws (small ws (t+w-1)) (small ws t) v in
    let frac = extract fs (small fs (t-1)) (small fs 0) v in
    let ones = small ws ~-1 in
    let zero = small fs 0 in
    let is_fin = inv (eq expn ones) in
    let is_sub = eq expn (small ws 0) in
    let is_pos = msb v in
    ite is_fin
      (fin ~is_sub)
      (ite (eq frac zero) (inf ~is_pos)
         (nan ~is_tss:(msb frac)))


  let tmp x f =
    x >>= fun x ->
    Theory.Var.scoped (sort x) @@ fun v ->
    let_ v !!x (f (var v))


  let make_cast_float cast s m v =
    match ieee754_of_sort s with
    | None -> CT.unk s
    | Some p ->
      cast (Theory.IEEE754.Sort.define p) m v >>| resort s

  let cast_float s m v = make_cast_float Float.cast_float s m v
  let cast_sfloat s m v = make_cast_float Float.cast_float_signed s m v

  let cast_int ts _ v =
    v >>= fun v ->
    with_fsort ~unk_s:ts (sort v) @@ fun bs s ->
    Float.cast_int s ts !!(resort bs v)

  let cast_sint = cast_int

  let case f x =
    x >>= fun x ->
    with_fsort (sort x) ~unk_s:bool @@ fun bs fs ->
    f fs !!(resort bs x)

  let is_finite x = case Float.is_finite x
  let is_nan x = case Float.is_nan x
  let is_inf x = case Float.is_inf x
  let is_fzero x = case (fun _ -> Float.is_zero) x
  let is_fpos x = case (fun _ -> Float.is_fpos) x
  let is_fneg x = case (fun _ -> Float.is_fneg) x

  let forder x y =
    x >>= fun x ->
    y >>= fun y ->
    let xs = sort x in
    match ieee754_of_sort xs with
    | None -> CT.unk bool
    | Some ({Theory.IEEE754.k; w; t}) ->
      let bs = bits k and ms = bits (k-1)in
      let x = resort bs x and y = resort bs y in
      let ws = bits w and fs = bits t in
      let ones = small ws ~-1 in
      let zero = small fs 0 in
      let expn v = extract ws (small ws (t+w-1)) (small ws t) v in
      let frac v = extract fs (small fs (t-1)) (small fs 0) v in
      let magn v = extract ms (small ms (k-2)) (small ms 0) v in
      let not_nan v = or_ (neq (expn v) ones) (neq (frac v) zero) in
      tmp (magn !!x) @@ fun mx ->
      tmp (magn !!y) @@ fun my ->
      tmp (msb !!x) @@ fun x_is_neg ->
      tmp (msb !!y) @@ fun y_is_neg ->
      let x_is_pos = inv x_is_neg and y_is_pos = inv y_is_neg in
      List.reduce_exn ~f:and_ [
        not_nan !!x;
        not_nan !!y;
        inv (and_ (is_zero mx) (is_zero my));
        inv (and_ x_is_pos y_is_neg);
        or_
          (and_ x_is_neg y_is_pos)
          (ite x_is_neg (ult my mx) (ult mx my))
      ]
end

module Core = CT
module Core_with_fp_emulation = struct
  include Core
  include Emulator
end
