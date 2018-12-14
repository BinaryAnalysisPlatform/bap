open Core_kernel
open Bap.Std
open Bap_knowledge
open Bap_core_theory

[@@@warning "-40"]


let exp = Bil.Domain.exp
let stmt = Bil.Domain.bil

let values = exp
let effects = stmt
let bool = Bool.t
let bits = Bits.define


module Basic : Theory.Basic = struct
  open Knowledge.Syntax

  module Base = struct

    module V = Bap.Std.Var

    let simpl = ident
    let ret = Knowledge.return

    let v s e = Value.put exp (Value.empty s) e
    let (%:) e s = v s e

    let bit x = ret @@ Some (simpl x) %: bool
    let exp s v = ret @@ Some (simpl v) %: s
    let unk s = ret @@ match Bool.cast s with
      | Some _ -> Some (Bil.unknown "bits" bool_t) %: s
      | None -> match Bits.cast s with
        | Some b ->
          Some (Bil.unknown "bits" (Type.imm (Bits.size b))) %: s
        | None -> None %: s

    let eff k d = ret @@ Eff.put stmt (Eff.empty k) d
    let data = eff Kind.data
    let ctrl = eff Kind.ctrl

    (* Bitvectors and booleans are always expressible in Bil.exp.
       Some memories might not be expressible (we size of an address
       part shall be 32 or 64, while the size of value should be
       8,16,32, or 64. Everything else is not expressible.

       The two functions below might fire an assertion inside of this
       module, e.g., when an unknown value is not created with the
       [unk] function above.*)
    let bool_exp : bit value -> Bil.exp = fun v ->
      match Value.get values v with
      | None -> Unknown ("bool", bool_t)
      | Some x -> x

    let bitv_exp : _ bitv value -> Bil.exp = fun v ->
      match Value.get values v with
      | None -> Unknown ("word", Type.Imm (Bits.size (Value.sort v)))
      | Some x -> x

    let sort v = v >>| fun v -> Value.sort v

    let type_of_sort s = Bits.cast s |> function
      | Some b -> Some (Type.imm (Bits.size b))
      | None -> Bool.cast s |> function
        | Some _ -> Some bool_t
        | None -> Mems.cast s |> function
          | None -> None
          | Some ms ->
            let ks = Mems.keys ms and vs = Mems.vals ms in
            let ks = Size.addr_of_int_opt (Bits.size ks) in
            let vs = Size.of_int_opt (Bits.size vs) in
            match ks, vs with
            | Some ks, Some vs -> Some (Type.mem ks vs)
            | _ -> None

    let reify_to_var r =
      match type_of_sort (Var.sort r) with
      | None -> None
      | Some t ->
        let is_virtual = Var.is_virtual r in
        Some (V.create ~is_virtual (Var.name r) t)

    let var r =
      let s = Var.sort r in
      match reify_to_var r with
      | None -> unk s
      | Some v -> exp s (Var v)

    let b0 = bit Bil.(int Word.b0)
    let b1 = bit Bil.(int Word.b1)

    let int s w =
      let order = Int.compare (Bits.size s) (Word.bitwidth w) in
      exp s @@ match Ordering.of_int order with
      | Equal -> Bil.(int w)
      | Less -> Bil.(cast low (Bits.size s) (int w))
      | Greater -> Bil.(cast unsigned (Bits.size s) (int w))

    let value x = Value.get values x
    let vsort x = Value.sort x
    let effect x = Eff.get effects x
    let esort x = Eff.kind x

    let (>>->) v f = v >>= fun v -> f (vsort v) (value v)

    let lift1 s f v = v >>-> fun sort -> function
      | None -> unk (s sort)
      | Some x -> exp (s sort) (f x)

    let lift2 s f x y =
      x >>-> fun sx x ->
      y >>-> fun sy y ->
      match x, y with
      | Some x, Some y -> exp (s sx sy) (f x y)
      | _ -> unk (s sx sy)

    let lift3 s f x y z =
      x >>-> fun sx x ->
      y >>-> fun sy y ->
      z >>-> fun sz z ->
      match x, y, z with
      | Some x, Some y, Some z -> exp (s sx sy sz) (f x y z)
      | _ -> unk (s sx sy sz)


    (* typing rules *)
    let t_lo1 : 'a sort -> bit sort = fun _ -> bool
    let t_lo2 : 'a sort -> 'a sort -> bit sort = fun _ _ -> bool
    let t_uop : 'a sort -> 'a sort = fun x -> x
    let t_aop : 'a sort -> 'a sort -> 'a sort = fun x _ -> x
    let t_sop : 'a sort -> 'b sort -> 'a sort = fun x _ -> x

    (* operators *)
    let lo1 x = lift1 t_lo1 x
    let lo2 x y = lift2 t_lo2 x y
    let uop x = lift1 t_uop x
    let aop x y = lift2 t_aop x y
    let sop x y = lift2 t_sop x y

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

    let shiftr b x y =
      b >>-> fun _s b ->
      x >>-> fun xs x ->
      y >>-> fun _ y ->
      match b,x,y with
      | Some b, Some x, Some y ->
        exp xs @@
        if Exp.equal b (Bil.int Word.b0) then Bil.(x lsr y)
        else
          let ones = Word.ones (Bits.size xs) in
          let mask = Bil.(lnot (int ones lsr y)) in
          Bil.(ite b ((x lsr y) lor mask) (x lsr y))
      | _ -> unk xs

    let shiftl b x y =
      b >>-> fun _s b ->
      x >>-> fun xs x ->
      y >>-> fun _ y ->
      match b,x,y with
      | Some b, Some x, Some y ->
        exp xs @@
        if Exp.equal b (Bil.int Word.b0) then Bil.(x lsl y)
        else
          let ones = Word.ones (Bits.size xs) in
          let mask = Bil.(lnot (int ones lsl y)) in
          Bil.(ite b ((x lsl y) lor mask) (x lsl y))
      | _ -> unk xs

    let app_bop lift op x y = lift (Bil.binop op) x y
    let arshift x y = app_bop sop Bil.arshift x y
    let rshift x y = app_bop sop Bil.rshift x y
    let lshift x y = app_bop sop Bil.lshift x y

    let ite cnd yes nay =
      cnd >>= fun cnd ->
      yes >>-> fun s yes ->
      nay >>-> fun _ nay ->
      match yes,nay with
      | Some yes, Some nay -> exp s (Bil.ite (bool_exp cnd) yes nay)
      | _ -> unk s

    let (>>:=) v f = v >>= fun v -> f (Eff.get effects v)

    let branch cnd yes nay =
      cnd >>= fun cnd ->
      yes >>= fun yes ->
      nay >>:= fun nay ->
      eff (esort yes) Bil.[If (bool_exp cnd,effect yes,nay)]

    let make_cast s t x =
      x >>-> fun _ -> function
      | Some x -> exp s Bil.(cast t (Bits.size s) x)
      | None -> unk s

    let high s = make_cast s Bil.high
    let low s = make_cast s Bil.low
    let signed s = make_cast s Bil.signed
    let unsigned s = make_cast s Bil.unsigned

    let size = Bits.size

    let mask_high res n =
      let width = size res in
      let n = Word.of_int ~width n in
      int res Word.(lnot (ones width lsr n))

    let cast res b x =
      x >>= fun src ->
      b >>= fun fill ->
      let sort = vsort src in
      let src = bitv_exp src in
      let fill = bool_exp fill in
      let diff = size res - size sort in
      let cast kind = exp res Bil.(Cast (kind,size res,src)) in
      match compare diff 0,fill with
      | 0,_ -> exp res src
      | 1, Bil.Int b ->
        if Word.(b = b0) then cast UNSIGNED
        else ite (msb x)
            (cast SIGNED)
            (logor (cast UNSIGNED) (mask_high res diff))
      | 1, _ ->
        ite b
          (logor (cast UNSIGNED) (mask_high res diff))
          (cast UNSIGNED)
      | _ -> exp res (Cast (LOW,size res,src))

    let append s x y =
      x >>= fun ex ->
      y >>= fun ey ->
      let sx = vsort ex and sy = vsort ey in
      let x = bitv_exp ex and y = bitv_exp ey in
      match compare (size sx + size sy) (size s) with
      | 0 -> exp s (Concat (x,y))
      | 1 ->
        let extra = size s - size sx - size sy in
        exp s @@ Cast(UNSIGNED,extra,(Concat (x,y)))
      | _ ->
        if size s < size sx
        then exp s (Cast (LOW,size s, x))
        else exp s (Cast (LOW,size s, Concat (x,y)))

    let rec uncat acc : Bil.exp -> Bil.exp list = function
      | Concat ((Concat (x,y)), z) -> uncat (y::z::acc) x
      | Concat (x,y) -> x::y::acc
      | x -> x::acc

    let concat s vs = match vs with
      | [] -> unk s
      | _ ->
        Knowledge.List.all vs >>= fun vs ->
        let sz = List.fold ~init:0 vs ~f:(fun sz x ->
            sz + Bits.size (vsort x)) in
        let x = List.reduce_exn ~f:(fun x y -> Bil.Concat (x,y)) @@
          List.map vs ~f:bitv_exp in
        cast s b0 (exp (bits sz) x)

    let load mem key =
      mem >>-> fun sort mem ->
      key >>-> fun _ key ->
      let vals = Mems.vals sort in
      match mem,key with
      | Some mem, Some key when Bits.size vals = 8 ->
        exp vals Bil.(load mem key BigEndian `r8)
      | _ -> unk vals

    let store m k d =
      m >>-> fun ms m ->
      k >>-> fun _ k ->
      d >>-> fun ds d ->
      match m, k, d, size (Mems.vals ms), size ds with
      | Some m, Some k, Some d, 8, 8 ->
        exp ms Bil.(store ~mem:m ~addr:k d BigEndian `r8)
      | _ -> unk ms

    let pass = data []
    let skip = ctrl []

    let seq x y =
      x >>= fun x ->
      y >>= fun y ->
      eff (esort x) (effect x @ effect y)

    let blk _ x y =
      x >>:= fun x ->
      y >>:= fun y ->
      eff Kind.unit (x @ y)

    let let_ var rhs body =
      rhs >>-> fun _ rhs ->
      body >>-> fun sort body ->
      match reify_to_var var, rhs, body with
      | None,_,_| _,None,_ | _,_,None -> unk sort
      | Some var, Some rhs, Some body -> exp sort @@ Let (var,rhs,body)

    let set var rhs =
      rhs >>-> fun _ rhs ->
      match reify_to_var var, rhs with
      | Some var, Some exp -> data [Bil.Move (var,exp)]
      | _ ->  data [
          Bil.special @@ sprintf "(set %s <?>)" (Var.name var)
        ]

    let repeat cnd body =
      cnd >>= fun cnd ->
      body >>:= fun body ->
      data [Bil.While (bool_exp cnd, body)]

    let jmp dst =
      dst >>= fun dst -> ctrl [Bil.Jmp (bitv_exp dst)]

    let goto lbl = ctrl [
        Bil.special @@ Format.asprintf "(goto %a)" Label.pp lbl
      ]

    let atomic s = s
    let mfence = pass
    let lfence = pass
    let sfence = pass
  end

  include Theory.Basic.Make(Base)
  include Base

  let loadw rs cnd mem key =
    match Size.of_int_opt (size rs) with
    | None -> loadw rs cnd mem key
    | Some sz ->
      cnd >>| bool_exp >>= fun dir ->
      key >>| bitv_exp >>= fun key ->
      mem >>-> fun _ -> function
      | None -> unk rs
      | Some mem ->
        let bel = exp rs @@ Load (mem,key,BigEndian,sz)
        and lel = exp rs @@ Load (mem,key,LittleEndian,sz) in
        match dir with
        | Int dir -> if Word.(dir = b1) then bel else lel
        | _ -> ite cnd bel lel

  let storew cnd mem key elt =
    elt >>-> fun es e ->
    match e, Size.of_int_opt (size es) with
    | _,None |None,_ -> storew cnd mem key elt
    | Some elt, Some sz ->
      cnd >>| bool_exp >>= fun dir ->
      key >>| bitv_exp >>= fun key ->
      mem >>-> fun sort -> function
      | None -> unk sort
      | Some mem ->
        let bes = exp sort @@ Store (mem,key,elt,BigEndian,sz)
        and les = exp sort @@ Store (mem,key,elt,LittleEndian,sz) in
        match dir with
        | Int dir -> if Word.(dir = b1) then bes else les
        | _ -> ite cnd bes les

  let extract s hi lo x =
    hi >>= fun hi ->
    lo >>= fun lo ->
    x  >>= fun x ->
    let get x = Value.get values x in
    match get hi,get lo, get x with
    | Some (Int h), Some (Int l), Some e ->
      let h = Word.to_int_exn h
      and l = Word.to_int_exn l in
      if h - l + 1 = size s
      then exp s @@ Bil.(extract ~hi:h ~lo:l e)
      else extract s !!hi !!lo !!x
    | _ -> extract s !!hi !!lo !!x

  let goto lbl : ctrl eff knowledge =
    Link.resolve Link.addr lbl >>= function
    | Some addr -> ctrl Bil.[Jmp (Int addr)]
    | None -> Link.resolve Link.ivec lbl >>= function
      | Some ivec -> ctrl Bil.[CpuExn ivec]
      | None -> Link.resolve Link.name lbl >>= fun name ->
        let dst = match name with
          | Some name -> sprintf "(call %s)" name
          | None -> (Format.asprintf "(goto %a)" Label.pp lbl) in
        ctrl Bil.[Special dst]
end


module BIL : Theory.Core = struct
  include Theory.Core.Empty
  include Basic
end

module FBil = Bil_float.Make(Basic)

module FPEmulator = struct
  open Knowledge.Syntax
  type 'a t = 'a knowledge
  type 'a float = 'a Bap_core_theory.float
  type 'a value = 'a Bap_core_theory.value

  let supported = IEEE754.[
      binary16;
      binary32;
      binary64;
      binary80;
      binary128;
    ]

  let ieee754_of_sort s =
    List.find supported ~f:(fun p ->
        Sort.same s (IEEE754.Sort.define p))

  let resort s v = Value.create s (Value.semantics v)

  let fbits x =
    x >>| fun x -> resort (Floats.size (Value.sort x)) x

  let float s x =
    x >>| fun x -> resort s x


  let fop : type f.
    _ ->
    _ -> f float value t -> f float value t -> f float value t =
    fun op rm x y ->
      x >>= fun x ->
      y >>= fun y ->
      let xs = Value.sort x in
      match ieee754_of_sort xs with
      | None -> BIL.unk xs
      | Some ({IEEE754.k} as p) ->
        let bs = Bits.define k in
        let x = resort bs x and y = resort bs y in
        let s = IEEE754.Sort.define p in
        float xs (op s rm !!x !!y)

  let fadd rm = fop FBil.fadd rm
  let fsub rm = fop FBil.fsub rm
  let fmul rm = fop FBil.fmul rm
  let fdiv rm = fop FBil.fdiv rm

  open BIL

  let small s x = int s (Word.of_int ~width:(Bits.size s) x)

  let classify {IEEE754.w; t} v ~fin ~inf ~nan =
    let ws = Bits.define w and fs = Bits.define t in
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
    Var.Generator.fresh (Value.sort x) >>= fun v ->
    let_ v !!x (f (var v))


  let forder x y =
    x >>= fun x ->
    y >>= fun y ->
    let xs = Value.sort x in
    match ieee754_of_sort xs with
    | None -> BIL.unk bool
    | Some ({IEEE754.k; w; t}) ->
      let bs = Bits.define k and ms = Bits.define (k-1)in
      let x = resort bs x and y = resort bs y in
      let ws = Bits.define w and fs = Bits.define t in
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

module BIL_FP = struct
  include BIL
  include FPEmulator
end


let init () = Theory.register
    ~desc:"denotes programs in terms of BIL expressions and statements"
    ~name:"bil"
    (module BIL_FP)
