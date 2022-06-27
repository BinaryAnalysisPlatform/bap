open Core_kernel[@@warning "-D"]
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Bap_common
open Format
open Bap_bil

module Var = Bap_var
module Word = Bitvector
module Size = Bap_size
module Type_error = Bap_type_error

module PP = struct
  open Bap_bil

  let pp_cast fmt cst = fprintf fmt "%s"
      (match cst with
       | Cast.UNSIGNED -> "pad"
       | Cast.SIGNED   -> "extend"
       | Cast.HIGH     -> "high"
       | Cast.LOW      -> "low")

  let pp_binop fmt op = fprintf fmt "%s"
      Binop.(match op with
          | PLUS    -> "+"
          | MINUS   -> "-"
          | TIMES   -> "*"
          | DIVIDE  -> "/"
          | SDIVIDE -> "/$"
          | MOD     -> "%"
          | SMOD    -> "%$"
          | LSHIFT  -> "<<"
          | RSHIFT  -> ">>"
          | ARSHIFT -> "~>>"
          | AND     -> "&"
          | OR      -> "|"
          | XOR     -> "^"
          | EQ      -> "="
          | NEQ     -> "<>"
          | LT      -> "<"
          | LE      -> "<="
          | SLT     -> "<$"
          | SLE     -> "<=$")

  let pp_unop fmt op = fprintf fmt "%s"
      Unop.(match op with
          | NEG -> "-"
          | NOT -> "~")

  let pp_edn fmt e = fprintf fmt "%s"
      Bap_bil.(match e with
          | LittleEndian -> "el"
          | BigEndian    -> "be")

  type precendence = int

  let op_prec op = Binop.(match op with
      | TIMES | DIVIDE | SDIVIDE | MOD| SMOD -> 8
      | PLUS | MINUS -> 7
      | LSHIFT | RSHIFT | ARSHIFT -> 6
      | LT|LE|SLT|SLE -> 5
      | EQ|NEQ -> 4
      | AND -> 3
      | XOR -> 2
      | OR -> 1)

  let prec x = Exp.(match x with
      | Var _ | Int _ | Unknown _ -> 10
      | Load _ | Cast _ | Extract _ -> 10
      | UnOp _ -> 9
      | BinOp (op,_,_) -> op_prec op
      | Store _ | Let _ | Ite _ | Concat _ -> 0)

  let msb x =
    let m = Bitvec.modulus (Word.bitwidth x) in
    Bitvec.(msb (Word.to_bitvec x) mod m)

  let rec pp fmt exp =
    let open Bap_bil.Exp in
    let open Bap_bil.Binop in
    let open Bap_bil.Unop in
    let pfmt p c =
      if prec c >= prec p
      then format_of_string "%a"
      else format_of_string "(@[<2>%a@])" in
    let pr s = fprintf fmt s in
    let is_b0 x = Bitvector.(x = b0) in
    let is_b1 x = Bitvector.(x = b1) in
    match exp with
    | Load (mem, idx, _, `r8) ->
      pr "%a[%a]" pp mem pp idx
    | Load (mem, idx, edn, s) ->
      pr "%a[%a, %a]:%a" pp mem pp idx pp_edn edn Bap_size.pp s
    | Store (mem, idx, exp, _, `r8) ->
      pr "@[<4>%a@;with [%a] <- %a@]"
        pp mem pp idx pp exp
    | Store (mem, idx, exp, edn, s) ->
      pr "@[<4>%a@;with [%a, %a]:%a <- %a@]"
        pp mem pp idx pp_edn edn Bap_size.pp s pp exp
    | Ite (ce, te, fe) ->
      pr "@[if %a@;then %a@;else %a@]" pp ce pp te pp fe
    | Extract (hi, lo, exp) ->
      pr "%d:%d[%a]" hi lo pp exp
    | Concat (le, re) as p ->
      pr (pfmt p le ^^ "." ^^ pfmt p re) pp le pp re
    | BinOp (EQ,e, Int x) when is_b1 x -> pr ("%a") pp e
    | BinOp (EQ,Int x, e) when is_b1 x -> pr ("%a") pp e
    | BinOp (EQ,e, Int x) as p when is_b0 x ->
      pr ("%a" ^^ pfmt p e) pp_unop Unop.NOT pp e
    | BinOp (EQ,Int x, e) as p when is_b0 x ->
      pr ("%a" ^^ pfmt p e) pp_unop Unop.NOT pp e
    | BinOp (PLUS,le,(Int x as re)) as p when msb x ->
      pr (pfmt p le ^^ " - " ^^ pfmt p re) pp le Word.pp (Word.neg x)
    | BinOp (op, le, re) as p ->
      pr (pfmt p le ^^ "@ %a@ " ^^ pfmt p re) pp le pp_binop op pp re
    | UnOp (op, exp) as p ->
      pr ("%a" ^^ pfmt p exp) pp_unop op pp exp
    | Var var -> Bap_var.pp fmt var
    | Int bv  -> pr "%a" Bap_bitvector.pp_hex bv
    | Cast (ct, n, exp) ->
      pr "%a:%d[%a]" pp_cast ct n pp exp
    | Let (var, def, body) ->
      pr "@[let %a =@ %a in@ %a@]" Bap_var.pp var pp def pp body
    | Unknown (s, typ) ->
      pr "unknown[%s]:%a" s Bap_type.pp typ
end

(* maps BIL expressions to Word operations *)
module Apply = struct
  open Bap_bil
  open Binop
  open Unop
  let is_shift = function
    | LSHIFT | RSHIFT | ARSHIFT -> true
    | _ -> false

  let unop op u = match op with
    | NEG -> Word.neg u
    | NOT -> Word.lnot u

  let binop op u v =
    let open Word in
    match op with
    | LSHIFT -> u lsl v
    | RSHIFT -> u lsr v
    | ARSHIFT -> u asr v
    | _ ->
      let hi = Int.(max (bitwidth u) (bitwidth v) - 1)  in
      let u = extract_exn ~hi u
      and v = extract_exn ~hi v in
      match op with
      | PLUS -> u + v
      | MINUS -> u - v
      | TIMES -> u * v
      | DIVIDE -> u / v
      | SDIVIDE -> signed u / signed v
      | MOD -> u mod v
      | SMOD -> signed u mod signed v
      | AND -> u land v
      | OR -> u lor v
      | XOR -> u lxor v
      | EQ -> Bitvector.(of_bool (u = v))
      | NEQ -> Bitvector.(of_bool (u <> v))
      | LT -> Bitvector.(of_bool (u < v))
      | LE -> Bitvector.(of_bool (u <= v))
      | SLT -> Bitvector.(of_bool (signed u < signed v))
      | SLE  -> Bitvector.(of_bool (signed u <= signed v))
      | (LSHIFT|RSHIFT|ARSHIFT) -> assert false

  let cast ct sz u =
    let ext = Bitvector.extract_exn in
    match ct with
    | Cast.UNSIGNED -> ext ~hi:Int.(sz - 1) u
    | Cast.SIGNED   -> ext ~hi:Int.(sz - 1) (Bitvector.signed u)
    | Cast.HIGH     -> ext ~lo:Int.(Bitvector.bitwidth u - sz) u
    | Cast.LOW      -> ext ~hi:Int.(sz - 1) u

  let extract hi lo x = Bitvector.extract_exn ~hi ~lo x
end

module Type = struct
  let type_equal t t' = Type.compare t t' = 0

  let rec infer : exp -> _ = function
    | Var v -> Var.typ v
    | Int x -> Type.Imm (Word.bitwidth x)
    | Unknown (_,t) -> t
    | Load (m,a,_,s) -> load m a s
    | Cast (c,s,x) -> cast c s x
    | Store (m,a,x,_,t) -> store m a x t
    | BinOp (op,x,y) -> binop op x y
    | UnOp (_,x) -> unop x
    | Let (v,x,y) -> let_ v x y
    | Ite (c,x,y) -> ite c x y
    | Extract (hi,lo,x) -> extract hi lo x
    | Concat (x,y) -> concat x y
  and unify x y =
    let t1 = infer x and t2 = infer y in
    if type_equal t1 t2 then t1
    else Type_error.expect t1 ~got:t2
  and let_ v x y =
    let t = Var.typ v and u = infer x in
    if type_equal t u then infer y
    else Type_error.expect t ~got:u
  and ite c x y = match infer c with
    | Type.Mem _ -> Type_error.expect_imm ()
    | Type.Imm 1 -> unify x y
    | t -> Type_error.expect (Type.Imm 1) ~got:t
  and unop x = match infer x with
    | Type.Mem _ -> Type_error.expect_imm ()
    | t -> t
  and binop op x y = match op with
    | LSHIFT|RSHIFT|ARSHIFT -> shift x y
    | _ -> match unify x y with
      | Type.Mem _ | Type.Unk -> Type_error.expect_imm ()
      | Type.Imm _ as t -> match op with
        | LT|LE|EQ|NEQ|SLT|SLE -> Type.Imm 1
        | _ -> t
  and shift x y = match infer x, infer y with
    | Type.Mem _,_ | _,Type.Mem _
    | Type.Unk,_ | _,Type.Unk -> Type_error.expect_imm ()
    | t, Type.Imm _ -> t
  and load m a r = match infer m, infer a with
    | (Type.Imm _|Unk),_ -> Type_error.expect_mem ()
    | _,(Type.Mem _|Unk) -> Type_error.expect_imm ()
    | Type.Mem (s,_),Type.Imm s' ->
      let s = Size.in_bits s in
      if s = s' then Type.Imm (Size.in_bits r)
      else Type_error.expect (Type.Imm s) ~got:(Type.Imm s')
  and store m a x _ =
    match infer m, infer a, infer x with
    | Type.Imm _,_,_ -> Type_error.expect_mem ()
    | Type.Mem (s,_) as t, Type.Imm s', Type.Imm u ->
      let s = Size.in_bits s in
      if s <> s'
      then Type_error.expect (Type.Imm s) ~got:(Type.Imm s')
      else if is_error (Size.of_int u)
      then Type_error.wrong_cast ()
      else t
    | _ -> Type_error.expect_imm ()
  and cast c s x =
    let t = Type.Imm s in
    match c,infer x with
    | _,(Type.Mem _|Unk) -> Type_error.expect_imm ()
    | (UNSIGNED|SIGNED),_ -> t
    | (HIGH|LOW), Type.Imm s' ->
      if s' >= s then t else Type_error.wrong_cast ()
  and extract hi lo x = match infer x with
    | Type.Mem _ | Unk -> Type_error.expect_imm ()
    | Type.Imm _ ->
      (* we don't really need a type of x, as the extract operation
         can both narrow and widen. Though it is a question whether it is
         correct or not, especially wrt to the operational semantics, the
         real life fact is that our lifters are (ab)using extract
         instruction in both directions.  *)
      if hi >= lo then Type.Imm (hi - lo + 1)
      else Type_error.wrong_cast ()
  and concat x y = match infer x, infer y with
    | Type.Imm s, Type.Imm t -> Type.Imm (s+t)
    | _ -> Type_error.expect_mem ()

  let infer_exn = infer
  let infer x =
    try Ok (infer x) with
    | Type_error.T err -> Error err
end

module Binop = struct
  open Binop

  let is_commutative = function
    | PLUS | TIMES | AND | XOR | OR | EQ | NEQ -> true
    | _ -> false

  let is_associative = function
    | PLUS | TIMES | AND | OR | XOR -> true
    | _ -> false

  let plus = PLUS
  let minus = MINUS
  let times = TIMES
  let divide = DIVIDE
  let sdivide = SDIVIDE
  let modulo  = MOD
  let smodulo = SMOD
  let lshift = LSHIFT
  let rshift = RSHIFT
  let arshift = ARSHIFT
  let bit_and = AND
  let bit_xor = XOR
  let bit_or = OR
  let eq = EQ
  let neq = NEQ
  let lt = LT
  let le = LE
  let slt = SLT
  let sle = SLE

  include PP
  let string_of_binop = asprintf "%a" pp_binop
end

module Exp = struct
  open Exp


  let is0 = Word.is_zero and is1 = Word.is_one
  let ism1 x = Word.is_zero (Word.lnot x)
  let app2 = Apply.binop

  (* we can't fail in smart constructors and should allow
     ill-formed BIL, which we should preserve for the later
     error reporting. *)
  let zero_or : exp -> exp = fun x -> match Type.infer x with
    | Ok Imm m -> Int (Word.zero m)
    | _ -> x

  let binop : binop -> exp -> exp -> exp = fun op x y ->
    let keep op x y = BinOp(op,x,y) in
    let int f = function Int x -> f x | _ -> false in
    let is0 = int is0 and is1 = int is1 and ism1 = int ism1 in
    let (=) x y = compare_exp x y = 0 in
    match op, x, y with
    | op, Int x, Int y -> Int (app2 op x y)
    | PLUS,BinOp(PLUS,x,Int y),Int z
    | PLUS,BinOp(PLUS,Int y,x),Int z ->
      BinOp(PLUS,x,Int (app2 PLUS y z))

    | PLUS,x,y  when is0 x -> y
    | PLUS,x,y  when is0 y -> x
    | MINUS,x,y when is0 x -> UnOp(NEG,y)
    | MINUS,x,y when is0 y -> x
    | MINUS,x,y when x = y -> zero_or (keep op x y)
    | MINUS,BinOp(MINUS,x,Int y), Int z ->
      BinOp(MINUS,x,Int (app2 PLUS y z))
    | MINUS,BinOp(PLUS,x,Int c1),Int c2
    | MINUS,BinOp(PLUS,Int c1,x),Int c2 ->
      BinOp(PLUS,x,Int (app2 MINUS c1 c2))

    | TIMES,x,_ when is0 x -> x
    | TIMES,_,y when is0 y -> y
    | TIMES,x,y when is1 x -> y
    | TIMES,x,y when is1 y -> x
    | (DIVIDE|SDIVIDE),x,y when is1 y -> x
    | (MOD|SMOD),x,y when is1 y -> zero_or (keep op x y)
    | (LSHIFT|RSHIFT|ARSHIFT),x,y when is0 y -> x
    | (LSHIFT|RSHIFT|ARSHIFT),x,_ when is0 x -> x
    | ARSHIFT,x,_ when ism1 x -> x
    | AND,x,_ when is0 x -> x
    | AND,_,y when is0 y -> y
    | AND,x,y when ism1 x -> y
    | AND,x,y when ism1 y -> x
    | AND,x,y when x = y -> x
    | OR,x,y  when is0 x -> y
    | OR,x,y  when is0 y -> x
    | OR,x,_  when ism1 x -> x
    | OR,_,y  when ism1 y -> y
    | OR,x,y  when x = y -> x
    | XOR,x,y when x = y -> zero_or (keep op x y)
    | XOR,x,y when is0 x -> y
    | XOR,x,y when is0 y -> x
    | EQ,x,y  when x = y -> Int Word.b1
    | NEQ,x,y when x = y -> Int Word.b0
    | (LT|SLT), x, y when x = y -> Int Word.b0
    | op,x,y -> keep op x y

  module Syntax = struct
    open Binop
    let ( + ) = binop plus
    let ( - ) = binop minus
    let ( * ) = binop times
    let ( / ) = binop divide
    let ( /$ ) = binop sdivide
    let ( mod ) = binop modulo
    let ( %$ ) = binop smodulo
    let ( lsl ) = binop lshift
    let ( lsr ) = binop rshift
    let ( asr ) = binop arshift
    let ( land ) a b   = binop bit_and a b
    let ( lor  ) a b   = binop bit_or  a b
    let ( lxor ) a b   = binop bit_xor a b
    let ( = )    a b   = binop eq  a b
    let ( <> )    a b   = binop neq a b
    let ( < )    a b   = binop lt  a b
    let ( > )    a b   = binop lt  b a
    let ( <= )    a b   = binop le  a b
    let ( >= )    a b   = binop le  b a
    let ( <$ )   a b   = binop slt a b
    let ( >$ )   a b   = binop slt b a
    let ( <=$ )  a b   = binop sle a b
    let ( >=$ )  a b   = binop sle b a
  end

  let unop : unop -> exp -> exp = fun op x -> match op,x with
    | op,Int x -> Int (Apply.unop op x)
    | op,UnOp(op',x) when [%compare.equal: unop] op op' -> x
    | NOT,BinOp(LT,x,y) -> Syntax.(x >= y)
    | NOT,BinOp(LE,x,y) -> Syntax.(x > y)
    | NOT,BinOp(SLT,x,y) -> Syntax.(x >=$ y)
    | NOT,BinOp(SLE,x,y) -> Syntax.(x >$ y)
    | NOT,BinOp(EQ,x,y) -> Syntax.(x <> y)
    | NOT,BinOp(NEQ,x,y) -> Syntax.(x = y)
    | op,x -> UnOp(op, x)

  let equal_cast = [%compare.equal: cast]

  let cast t s x = match x with
    | Cast (_,s',_) as x when s = s' -> x
    | Cast (t',s',x) when equal_cast t t' && s' >= s ->
      Cast (t,s,x)
    | Cast (UNSIGNED as t',s',x)
    | Cast (SIGNED as t',s',x) when equal_cast t t' && s' <= s ->
      Cast (t,s,x)
    | Extract(p,_,x) when equal_cast t HIGH ->
      Extract(p,p-s+1,x)
    | Int w -> Int (Apply.cast t s w)
    | x -> match Type.infer x with
      | Ok Imm m when m = s -> x
      | _ ->  Cast (t,s,x)

  let ite ~if_:c ~then_:x ~else_:y = match c with
    | Int c -> if Word.(c = b1) then x else y
    | _ -> Ite(c,x,y)

  let has_width n x = match Type.infer x with
    | Ok Imm m -> m = n
    | _ -> false

  let extract ~hi ~lo x = match x with
    | Int w -> Int (Word.extract_exn ~hi ~lo w)
    | Extract (p,q,x) when hi <= p && lo >= q ->
      Extract (hi,lo,x)
    | x when lo = 0 && has_width (hi + 1) x -> x
    | x -> Extract (hi,lo,x)

  let concat x y = match x, y with
    | Int x, Int y -> Int (Word.concat x y)
    | Cast (HIGH,s,(Var x as v)), Extract(p,q,Var x')
      when Var.equal x x' && has_width (p + s + 1) v ->
      cast HIGH (s + p - q + 1) v
    | Cast (HIGH,s,(Var x as v)), Concat ((Extract(p,q,Var x')),z)
      when Var.equal x x' && has_width (p + s + 1) v ->
      Concat (cast HIGH (s + p - q + 1) v, z)
    | x,y -> Concat (x,y)

  let var v = Var v
  let int w = Int w
  let let_ v e b = Let (v,e,b)
  let unknown s t = Unknown (s,t)

  let load_byte ~mem ~addr = Load (mem,addr,BigEndian,`r8)
  let store_byte ~mem ~addr value = Store (mem,addr,value,BigEndian,`r8)
  let load ~mem ~addr e s = Load (mem,addr,e,s)
  let store ~mem ~addr value e s = Store (mem,addr,value,e,s)

end
include Exp


module Unop = struct
  open Unop
  let neg = NEG
  let not = NOT

  let pp = PP.pp_unop
  include PP
  let string_of_unop = asprintf "%a" pp_unop
end

module Cast = struct
  open Cast
  let unsigned = UNSIGNED
  let signed = SIGNED
  let high = HIGH
  let low = LOW
  include PP
  let string_of_cast = asprintf "%a" pp_cast
end

module Infix = struct
  open Bap_bil.Exp
  open Binop
  open Unop
  include Exp.Syntax
  let lnot     a     = unop  not a
  (** Misc operations *)
  let ( ^ )    a b   = concat a b
end



let equal x y = phys_equal x y || compare_exp x y = 0
let to_string = Format.asprintf "%a" PP.pp
let domain = Knowledge.Domain.flat "exp" ~equal
    ~empty:(Unknown ("empty",Unk))
    ~inspect:(fun exp -> Sexp.Atom (to_string exp))

let persistent = Knowledge.Persistent.of_binable (module struct
    type t = Bap_bil.exp [@@deriving bin_io]
  end)

let slot = Knowledge.Class.property ~package:"bap"
    ~persistent Theory.Value.cls  "exp" domain
    ~public:true
    ~desc:"semantics of expressions in BIL"


include Regular.Make(struct
    type t = Bap_bil.exp [@@deriving bin_io, compare, sexp]
    let hash = Hashtbl.hash
    let module_name = Some "Bap.Std.Exp"
    let version = "1.0.0"
    let pp = PP.pp
  end)
