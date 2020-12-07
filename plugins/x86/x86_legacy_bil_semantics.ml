open Core_kernel
open Bap_core_theory
open Bap_knowledge

open X86_legacy_bil
open X86_legacy_bil_pp

type exp = Ast.exp

type stmt = Ast.stmt
type rmode = Type.roundmode_type
module Bil = Ast
include Bap.Std.Self()

[@@@warning "-40"]

let package = "bap"

let rm_unused = Type.RNE (* a rounding mode that is not used *)

module Binary = struct
  let to_bv k x = Bil.UnOp(FP (FFTOIEEEBV k,rm_unused),x)
  let of_bv s n e k  =
    Bil.UnOp (FP (FIEEEBVTOF s, rm_unused),
              Concat (n, Concat (e,k)))

  let fextract ?bias {Theory.IEEE754.w; p; k} x ~hi ~lo =
    let hi = Z.of_int hi and lo = Z.of_int lo in
    let x = Bil.Extract (hi, lo, to_bv k x) in
    match bias with
    | None -> x
    | Some b -> BinOp(MINUS,x,b)

  let unbiased_exponent ({Theory.IEEE754.bias; t; w} as p) x =
    fextract p x ~hi:(t + w) ~lo:t
      ~bias:(Int (Z.of_int (bias + t), Reg w))

  let unbiased_exponent ({Theory.IEEE754.t; w} as p) x = fextract p x
      ~hi:(t + w)
      ~lo:t

  let fraction ({Theory.IEEE754.t} as p) x = fextract p x
      ~hi:(t - 1)
      ~lo:0

  let sign ({Theory.IEEE754.k} as p) x = fextract p x ~hi:(k-1) ~lo:(k-1)

  let mk_nan {Theory.IEEE754.w; t} is_quiet =
    let b0 = Bil.Int (Z.zero, Reg 1) in
    let ones = Bil.Int (Z.minus_one, Reg w) in
    let payload = Bil.Unknown ("nan-payload", Reg (t-1)) in
    Bil.Concat (b0, Concat(ones, Concat (is_quiet, payload)))

  let is_nan ({Theory.IEEE754.w; t} as p) x =
    let uexp = unbiased_exponent p x in
    let frac = fraction p x in
    let ones = Bil.Int (Z.minus_one, Reg w) in
    let zero = Bil.Int (Z.zero, Reg t) in
    Bil.BinOp (AND,
               BinOp (EQ, uexp, ones),
               BinOp (NEQ, frac, zero))
end

let bits_of_var (_,_,t) = match t with
  | Type.Reg x -> x
  | _ -> invalid_arg "expects a bitvector"

let byte x = Ast.Int (Z.of_int x, Reg 8)

let var_name i v = sprintf "%s%d" v i

module Semantics = struct
  open Knowledge.Syntax
  include Theory.Empty

  module V = Bil.Var
  module Var = Theory.Var

  let var = Knowledge.Class.declare ~package "var" ()
  let lbl = Knowledge.Class.declare ~package "lbl" ()

  let create_var name typ =
    Knowledge.Symbol.intern ~package name var >>| fun obj ->
    let id = Knowledge.Object.id obj in
    V.V (Int63.to_int_exn id, name, typ)

  let fresh_var name typ =
    Knowledge.Object.create var >>| fun obj ->
    V.V (Int63.to_int_exn (Knowledge.Object.id obj),name,typ)

  let fresh_label =
    Knowledge.Object.create lbl >>=
    Knowledge.Object.repr lbl

  module Domain = struct
    let stmt = Knowledge.Domain.flat "stmt"
        ~empty:[]
        ~inspect:(function
            | [] -> Sexp.List []
            | prog -> Sexp.Atom (ast_prog_to_string prog))
        ~equal:Poly.(=)

    let exp = Knowledge.Domain.optional "exp"
        ~inspect:(fun exp ->
            Sexp.Atom (ast_exp_to_string exp))
        ~equal:Poly.(=)
  end

  let stmt = Knowledge.Class.property
      Theory.Semantics.cls "legacy-bil-stmt" Domain.stmt
      ~public:true
      ~package

  let exp = Knowledge.Class.property
      Theory.Value.cls "legacy-bil-exp" Domain.exp
      ~public:true
      ~package

  let values = exp
  let effects = stmt

  module Base = struct
    module KB = Knowledge
    let simpl = ident
    let ret = Knowledge.return
    let bool = Theory.Bool.t
    let bool_t = Type.Reg 1

    module Bil = Ast

    let value x = KB.Value.get exp x

    let v s e = KB.Value.put exp (Theory.Value.empty s) e
    let (%:) e s = v s e

    let bit x = ret @@ Some (simpl x) %: bool
    let exp s v = ret @@ Some (simpl v) %: s
    let unk s' =
      let s = Theory.Value.Sort.forget s' in
      ret @@ match Theory.Bool.refine s with
      | Some _ -> Some (Bil.Unknown ("bits", bool_t)) %: s'
      | None -> match Theory.Bitv.refine s with
        | Some b ->
          let size = Theory.Bitv.size b in
          Some (Ast.Unknown ("unk-bits", (Reg size))) %: s'
        | None -> None %: s'

    let empty = Theory.Effect.empty Theory.Effect.Sort.bot

    let eff d = ret @@ KB.Value.put stmt empty d
    let data s = eff s
    let ctrl s = eff s
    let sort = Theory.Value.sort
    let size = Theory.Bitv.size
    let bits = Theory.Bitv.define

    let bool_exp : _ -> Bil.exp = fun x ->
      match value x with
      | None -> Unknown ("bool", bool_t)
      | Some x -> x

    let bitv_exp : _ -> Bil.exp = fun x ->
      match value x with
      | None -> Unknown ("word", Reg (size (sort x)))
      | Some x -> x

    let type_of_sort s =
      let s = Theory.Value.Sort.forget s in
      Theory.Bitv.refine s |> function
      | Some b -> Some (Type.Reg (size b))
      | None -> Theory.Bool.refine s |> function
        | Some _ -> Some bool_t
        | None -> Theory.Mem.refine s |> function
          | None -> None
          | Some ms ->
            let ks = Theory.Mem.keys ms
            and vs = Theory.Mem.vals ms in
            let ks = Type.Reg (size ks) in
            let vs = Type.Reg (size vs) in
            Some (Type.TMem (ks,vs))

    let var r =
      let s = Var.sort r in
      match type_of_sort s with
      | None -> unk s
      | Some t -> create_var (Var.name r) t >>= fun v ->
        exp s (Var v)

    let zeros = Bil.Int (Z.of_int 0, Reg 1)
    let ones = Bil.Int (Z.of_int 1, Reg 1)

    let b0 = bit zeros
    let b1 = bit ones

    let int s w =
      exp s @@ Bil.Int (Bitvec.to_bigint w, Type.Reg (size s))

    let vsort x = sort x
    let effect x = KB.Value.get effects x
    let esort x = Theory.Effect.sort

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
    type 'a sort = 'a Theory.Value.sort
    let t_lo1 : 'a sort -> Theory.Bool.t sort = fun _ -> bool
    let t_lo2 : 'a sort -> 'a sort -> Theory.Bool.t sort = fun _ _ -> bool
    let t_uop : 'a sort -> 'a sort = fun x -> x
    let t_aop : 'a sort -> 'a sort -> 'a sort = fun x _ -> x
    let t_sop : 'a sort -> 'b sort -> 'a sort = fun x _ -> x

    let unop op x = Bil.UnOp (op,x)
    let binop op x y = Bil.BinOp (op,x,y)
    (* operators *)
    let lo1 op = lift1 t_lo1 (unop op)
    let lo2 op  = lift2 t_lo2 (binop op)
    let uop op = lift1 t_uop (unop op)
    let aop op = lift2 t_aop (binop op)
    let sop op = lift2 t_sop (binop op)

    let or_ x = lo2 Type.OR x
    let and_ x = lo2 Type.AND x

    let inv x = lo1 Type.NOT x
    let msb x = lift1 t_lo1 (fun x -> Bil.(Cast (CAST_HIGH, Reg 1,x))) x
    let lsb x = lift1 t_lo1 (fun x -> Bil.(Cast (CAST_LOW, Reg 1,x))) x
    let neg x = uop Type.NEG x
    let not   x = uop Type.NOT x
    let add x y = aop Type.PLUS x y
    let sub x y = aop Type.MINUS x y
    let mul x y = aop Type.TIMES x y
    let div x y = aop  Type.DIVIDE x y
    let sdiv x y = aop Type.SDIVIDE x y
    let modulo x y = aop Type.MOD x y
    let smodulo x y = aop Type.SMOD x y
    let logand x y = aop Type.AND x y
    let logor x y = aop Type.OR x y
    let logxor x y = aop Type.XOR x y
    let ule x y = lo2 Type.LE x y
    let sle x y = lo2 Type.SLE x y
    let eq x y = lo2 Type.EQ x y
    let neq x y = lo2 Type.NEQ x y
    let slt x y = lo2 Type.SLT x y
    let ult x y = lo2 Type.LT x y

    let (lsr) x y = Bil.BinOp (RSHIFT,x,y)
    let (lsl) x y = Bil.BinOp (LSHIFT,x,y)
    let (lor) x y = Bil.BinOp (OR,x,y)

    let lnot x = Bil.UnOp(NOT,x)

    let shiftr b x y =
      b >>-> fun _s b ->
      x >>-> fun xs x ->
      y >>-> fun _ y ->
      match b,x,y with
      | Some b, Some x, Some y ->
        exp xs @@
        if Poly.(=) b zeros then Bil.(BinOp(RSHIFT,x,y))
        else
          let ones = lnot Bil.(Int (Z.zero, Reg (size xs))) in
          let mask = lnot (ones lsr y) in
          Bil.(Ite (b, (x lsr y) lor mask, x lsr y))
      | _ -> unk xs

    let shiftl b x y =
      b >>-> fun _s b ->
      x >>-> fun xs x ->
      y >>-> fun _ y ->
      match b,x,y with
      | Some b, Some x, Some y ->
        exp xs @@
        if Poly.(=) b zeros then Bil.(x lsl y)
        else
          let ones = lnot Bil.(Int (Z.zero, Reg (size xs))) in
          let mask = Bil.(lnot (ones lsl y)) in
          Bil.(Ite (b, (x lsl y) lor mask, x lsl y))
      | _ -> unk xs


    let arshift x y = sop Type.ARSHIFT x y
    let rshift x y = sop Type.RSHIFT x y
    let lshift x y = sop Type.LSHIFT x y

    let ite cnd yes nay =
      cnd >>= fun cnd ->
      yes >>-> fun s yes ->
      nay >>-> fun _ nay ->

      match yes,nay with
      | Some yes, Some nay -> exp s (Bil.Ite (bool_exp cnd,yes,nay))
      | _ -> unk s

    let (>>:=) v f = v >>= fun v -> f (effect v)

    let branch cnd yes nay =
      cnd >>= fun cnd ->
      yes >>= fun yes ->
      nay >>:= fun nay ->
      fresh_label >>= fun lhs ->
      fresh_label >>= fun rhs ->
      fresh_label >>= fun bot ->
      eff begin Bil.[
          CJmp (bool_exp cnd, Lab lhs,[]);
          Jmp (Lab rhs,[]);
          Label (Name lhs,[])] @ effect yes @ Bil.[
          Jmp (Lab bot,[]);
          Label (Name rhs, []);
        ] @ nay @ Bil.[
          Label (Name bot, []);
        ]
      end

    let make_cast s t x =
      x >>-> fun _ -> function
      | Some x -> exp s Bil.(Cast (t,Reg (size s),x))
      | None -> unk s

    let high s = make_cast s Type.CAST_HIGH
    let low s = make_cast s Type.CAST_LOW
    let signed s = make_cast s Type.CAST_SIGNED
    let unsigned s = make_cast s Type.CAST_UNSIGNED

    let mask_high res n =
      let width = size res in
      let module Word = Bitvec.Make(struct
          let modulus = Bitvec.modulus width
        end) in
      let n = Word.int n in
      int res Word.(lnot (ones lsr n))

    let cast res b x =
      x >>= fun src ->
      b >>= fun fill ->
      let sort = vsort src in
      let src = bitv_exp src in
      let fill = bool_exp fill in
      let diff = size res - size sort in
      let cast kind = exp res Bil.(Cast (kind,Reg (size res),src)) in
      match compare diff 0,fill with
      | 0,_ -> exp res src
      | 1, b ->
        if Poly.(=) b zeros then cast CAST_UNSIGNED
        else ite (msb x)
            (cast CAST_SIGNED)
            (logor (cast CAST_UNSIGNED) (mask_high res diff))
      | _ -> exp res (Cast (CAST_LOW,Reg (size res),src))

    let append s ex ey =
      ex >>= fun ex ->
      ey >>= fun ey ->
      let sx = vsort ex and sy = vsort ey in
      let x = bitv_exp ex and y = bitv_exp ey in
      match compare (size sx + size sy) (size s) with
      | 0 -> exp s (Concat (x,y))
      | 1 ->
        let extra = size s - size sx - size sy in
        exp s @@ Cast(CAST_UNSIGNED,Reg extra,(Concat (x,y)))
      | _ ->
        if size s < size sx
        then exp s (Cast (CAST_LOW,Reg (size s), x))
        else exp s (Cast (CAST_LOW,Reg (size s), Concat (x,y)))

    let rec uncat acc : Bil.exp -> Bil.exp list = function
      | Concat ((Concat (x,y)), z) -> uncat (y::z::acc) x
      | Concat (x,y) -> x::y::acc
      | x -> x::acc

    let concat s vs = match vs with
      | [] -> unk s
      | _ ->
        Knowledge.List.all vs >>= fun vs ->
        let sz = List.fold ~init:0 vs ~f:(fun sz x ->
            sz + size (vsort x)) in
        let x = List.reduce_exn ~f:(fun x y -> Bil.Concat (x,y)) @@
          List.map vs ~f:bitv_exp in
        cast s b0 (exp (bits sz) x)

    let load mem key =
      mem >>-> fun sort mem ->
      key >>-> fun _ key ->
      let vals = Theory.Mem.vals sort in
      match mem,key with
      | Some mem, Some key when size vals = 8 ->
        exp vals Bil.(Load (mem,key,ones,Reg 8))
      | _ -> unk vals

    let store m k d =
      m >>-> fun ms m ->
      k >>-> fun _ k ->
      d >>-> fun ds d ->
      match m, k, d, size (Theory.Mem.vals ms), size ds with
      | Some m, Some k, Some d, 8, 8 ->
        exp ms Bil.(Store (m,k,d, ones, Reg 8))
      | _ -> unk ms

    let pass = data []
    let skip = ctrl []

    let seq x y =
      x >>= fun x ->
      y >>= fun y ->
      eff (effect x @ effect y)

    let blk _ x y =
      x >>:= fun x ->
      y >>:= fun y ->
      eff (x @ y)

    let let_ var rhs body =
      rhs >>-> fun _ rhs ->
      body >>-> fun sort body ->
      match type_of_sort (Var.sort var), rhs, body with
      | None,_,_| _,None,_ | _,_,None -> unk sort
      | Some t, Some rhs, Some body ->
        fresh_var (Var.name var) t >>= fun var ->
        exp sort @@ Let (var,rhs,body)

    let set var rhs =
      rhs >>-> fun _ rhs ->
      match type_of_sort (Var.sort var), rhs with
      | Some t, Some exp ->
        create_var (Var.name var) t >>= fun var ->
        data [Bil.Move (var,exp,[])]
      | _ ->  data [
          Bil.Special(sprintf "(set %s <?>)" (Var.name var),None,[])
        ]

    let repeat cnd body =
      cnd >>= fun cnd ->
      body >>= fun body ->
      fresh_label >>= fun head ->
      fresh_label >>= fun loop ->
      fresh_label >>= fun tail ->
      data begin Bil.[
          Label (Name head,[]);
          Jmp (Lab tail,[]);
          Label (Name loop,[]);
        ] @ effect body @ Bil.[
          Label (Name tail,[]);
          CJmp (bool_exp cnd, Lab head,[]);
        ]
      end

    let jmp dst =
      dst >>= fun dst -> ctrl [Bil.Jmp (bitv_exp dst, [])]

    let make_addr dst bitv =
      KB.collect Bap.Std.Arch.slot dst >>| fun arch ->
      let size = Bap.Std.Arch.addr_size arch in
      let data = Bitvec.to_bigint bitv in
      Bil.Int (data, Type.Reg (Bap.Std.Size.in_bits size))

    let goto lbl : _ knowledge =
      KB.collect Theory.Label.addr lbl >>= function
      | Some bitv ->
        make_addr lbl bitv >>= fun dst ->
        ctrl Bil.[Jmp (dst, [])]
      | None ->
        KB.collect Theory.Label.ivec lbl >>= function
        | Some ivec ->
          let ivec = sprintf "%%interrupt#%d" ivec in
          ctrl Bil.[Jmp (Lab ivec,[])]
        | None ->
          KB.collect Theory.Label.name lbl >>= fun name ->
          let dst = match name with
            | Some name -> sprintf "@@%s" name
            | None ->
              Format.asprintf "%%%08Lx"
                (Int63.to_int64 (KB.Object.id lbl)) in
          ctrl Bil.[Jmp (Lab dst,[])]

    let perform s = ret (Theory.Effect.empty s)
  end

  include Theory.Basic.Make(Base)
  include Base

  let loadw rs cnd mem key =
    cnd >>| bool_exp >>= fun dir ->
    key >>| bitv_exp >>= fun key ->
    mem >>-> fun _ -> function
    | None -> unk rs
    | Some mem -> exp rs @@ Load (mem,key,dir,Reg (size rs))

  let storew cnd mem key elt =
    elt >>-> fun es e ->
    match e with
    | None -> storew cnd mem key elt
    | Some elt ->
      cnd >>| bool_exp >>= fun dir ->
      key >>| bitv_exp >>= fun key ->
      mem >>-> fun sort -> function
      | None -> unk sort
      | Some mem -> exp sort @@ Store (mem,key,elt,dir,Reg (size es))

  let extract s hi lo x =
    hi >>= fun hi ->
    lo >>= fun lo ->
    x  >>= fun x ->
    let get x = KB.Value.get values x in
    match get hi,get lo, get x with
    | Some (Int (h,_)), Some (Int (l,_)), Some e ->
      exp s @@ Bil.(Extract (h,l,e))
    | _ -> extract s !!hi !!lo !!x

  let fsize s = Type.{
      exp_bits = size @@ Theory.IEEE754.Sort.exps s;
      sig_bits = size @@ Theory.IEEE754.Sort.sigs s;
    }

  let ftype s = Type.Float (fsize s)

  let to_bv k x = Bil.UnOp(FP (FFTOIEEEBV k,RNE),x)
  let of_bv s n e k  =
    Bil.UnOp (FP (FIEEEBVTOF (fsize s), RNE),
              Concat (n, Concat (e,k)))


  let known_ieee754_formats = Theory.IEEE754.[
      binary16;
      binary32;
      binary64;
      binary80;
      binary128;
    ]

  let ieee754_of_sort s =
    List.find known_ieee754_formats ~f:(fun p ->
        Theory.Value.Sort.same s (Theory.IEEE754.Sort.define p))

  let fsign x =
    x >>-> fun xs x -> match x with
    | None -> exp bool @@ Bil.Unknown ("float-sign", Reg 1)
    | Some x -> match ieee754_of_sort xs with
      | None -> exp bool @@ Bil.Unknown ("float-sign", Reg 1)
      | Some {Theory.IEEE754.k} ->
        exp bool @@ Cast (CAST_HIGH, Reg 1, to_bv k x)

  let with_ieee x ~ieee754 ~default =
    x >>-> fun xs x -> match x, ieee754_of_sort xs with
    | Some x, Some ({base=2} as p) -> ieee754 p x
    | _ -> default xs x

  let is_finite x = with_ieee x
      ~default:(fun _ _ -> exp bool @@ Unknown ("is-finite", Reg 1))
      ~ieee754:(fun ({w} as p) x ->
          let uexp = Binary.unbiased_exponent p x in
          let ones = Bil.Int (Z.minus_one, Reg w) in
          exp bool @@ BinOp (NEQ, uexp, ones))

  let is_fzero x = with_ieee x
      ~default:(fun _ _ -> exp bool @@ Unknown ("is-fzero", Reg 1))
      ~ieee754:(fun ({w; t} as p) x ->
          let uexp = Binary.unbiased_exponent p x in
          let frac = Binary.fraction p x in
          let zero s = Bil.Int (Z.zero, Reg s) in
          exp bool @@ BinOp (AND,
                             BinOp (EQ, uexp, zero w),
                             BinOp (EQ, frac, zero t)))

  let fsign x = with_ieee x
      ~default:(fun _ _ -> exp bool @@ Unknown ("fsign", Reg 1))
      ~ieee754:(fun p x -> exp bool @@ Binary.sign p x)


  let is_inf x = with_ieee x
      ~default:(fun _ _ -> exp bool @@ Unknown ("is-inf", Reg 1))
      ~ieee754:(fun ({w; t} as p) x ->
          let uexp = Binary.unbiased_exponent p x in
          let frac = Binary.fraction p x in
          let ones = Bil.Int (Z.minus_one, Reg w) in
          let zero = Bil.Int (Z.zero, Reg t) in
          exp bool @@ BinOp (AND,
                             BinOp (EQ, uexp, ones),
                             BinOp (EQ, frac, zero)))


  let is_pinf x = and_ (inv (fsign x)) (is_inf x)
  let is_ninf x = and_ (fsign x) (is_inf x)

  let is_nan x = with_ieee x
      ~default:(fun _ _ -> exp bool @@ Unknown ("is-nan", Reg 1))
      ~ieee754:(fun p x -> exp bool @@ Binary.is_nan p x)

  let is_snan = is_nan
  let is_qnan = is_nan


  let float s x = match ieee754_of_sort s with
    | Some ({Theory.IEEE754.base=2; w; p}) ->
      x >>| bitv_exp >>= fun x ->
      let fs = {Type.exp_bits = w; sig_bits = p} in
      exp s @@ UnOp (FP (FIEEEBVTOF fs, rm_unused),x)
    | _ -> ret (None %: s)


  let sortname s = Format.asprintf "%a" Theory.Value.Sort.pp s

  let fbits x =
    x >>-> fun fs x ->
    let bs = Theory.Float.bits fs in
    match ieee754_of_sort fs, x with
    | Some {base=2}, Some x ->
      exp bs @@ UnOp (FP (FFTOIEEEBV (size bs), rm_unused), x)
    | _ ->
      exp bs @@ Unknown (sortname fs, Reg (size bs))


  type 'a t = 'a knowledge

  let float_bitv_uop : type a.
    (Type.float_size -> Type.funop_type) ->
    _ sort -> _ -> _ t -> _ t =
    fun op s m x ->
    match ieee754_of_sort s with
    | Some {Theory.IEEE754.base=2; w; p} ->
      let fs = {Type.sig_bits = p; exp_bits = w} in
      x >>| bitv_exp >>= fun x ->
      exp s @@ UnOp (FP (op fs, RNE), x)
    | _ -> ret (None %: s)

  let bitv_float_uop : type a.
    (int -> Type.funop_type) ->
    _ sort -> _ -> _ t -> _ t  =
    fun op s m x ->
    x >>-> fun xs x -> match ieee754_of_sort s, x with
    | Some {Theory.IEEE754.base=2; k}, Some x ->
      exp s @@ UnOp (FP (op k, RNE), x)
    | _ ->
      ret (None %: s)

  let cast_float x = float_bitv_uop (fun fs -> FBVTOUF fs) x
  let cast_sfloat x = float_bitv_uop (fun fs -> FBVTOSF fs) x
  let cast_int x = bitv_float_uop (fun fs -> FFTOUBV fs) x
  let cast_sint x = bitv_float_uop (fun fs -> FFTOSBV fs) x

  let fuop op x =
    x >>-> fun xs x -> match ieee754_of_sort xs, x with
    | Some {Theory.IEEE754.base=2}, Some x ->
      exp xs @@ UnOp (FP (op,RNE), x)
    | _ -> ret (None %: xs)

  let fneg x = fuop FNEG x
  let fabs x = fuop FABS x
  let fround _ x = fuop FROUND x
  let fsqrt _ x = fuop FSQRT x

  let fbop op m x y =
    x >>-> fun xs x ->
    y >>-> fun ys y ->
    match ieee754_of_sort xs,x,y with
    | Some {Theory.IEEE754.base=2}, Some x, Some y ->
      exp xs @@ BinOp (FP (op, RNE),x, y)
    | _ -> ret (None %: xs)

  let fadd m = fbop FADD m
  let fsub m = fbop FSUB m
  let fmul m = fbop FMUL m
  let fdiv m = fbop FDIV m
  let fmodulo m = fbop FREM m

  (* since we've overwritten exp for expression :) *)
  let exp _ x =
    x >>-> fun xs _ -> ret (None %: xs)
end

module Legacy : Theory.Core = struct
  include Theory.Empty
  include Semantics
end


let init () =
  let open Knowledge.Syntax in
  Theory.declare
    ~package
    ~desc:"semantics in legacy AST"
    ~name:"legacy-bil"
    !!(module Legacy : Theory.Core)
