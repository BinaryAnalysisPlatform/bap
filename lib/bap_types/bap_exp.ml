open Core_kernel
open Regular.Std
open Bap_knowledge
open Bap_core_theory
open Bap_common
open Format
open Bap_bil

module Var = Bap_var
module Word = Bitvector
module Size = Bap_size

type binop = exp -> exp -> exp
type unop = exp -> exp


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
      pr "@[<2>if %a@;then %a@;else %a@]" pp ce pp te pp fe
    | Extract (hi, lo, exp) ->
      pr "extract:%d:%d[%a]" hi lo pp exp
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
      pr (pfmt p le ^^ " %a " ^^ pfmt p re) pp le pp_binop op pp re
    | UnOp (op, exp) as p ->
      pr ("%a" ^^ pfmt p exp) pp_unop op pp exp
    | Var var -> Bap_var.pp fmt var
    | Int bv  -> pr "%a" Bap_bitvector.pp_hex bv
    | Cast (ct, n, exp) ->
      pr "%a:%d[%a]" pp_cast ct n pp exp
    | Let (var, def, body) ->
      pr "let %a = %a in@ %a" Bap_var.pp var pp def pp body
    | Unknown (s, typ) ->
      pr "unknown[%s]:%a" s Bap_type.pp typ
end


module Exp = struct
  open Exp
  let load ~mem ~addr e s = Load (mem,addr,e,s)
  let store ~mem ~addr value e s = Store (mem,addr,value,e,s)
  let binop op x y = BinOp (op,x,y)
  let unop op x = UnOp (op,x)
  let var v = Var v
  let int w = Int w
  let cast ct s e = Cast (ct,s,e)
  let let_ v e b = Let (v,e,b)
  let unknown s t = Unknown (s,t)
  let ite ~if_ ~then_ ~else_ = Ite (if_,then_,else_)
  let extract ~hi ~lo e = Extract (hi,lo,e)
  let concat e1 e2 = Concat (e1,e2)
end
include Exp

module Binop = struct
  open Binop
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

  let is_commutative = function
    | PLUS | TIMES | AND | XOR | OR | EQ | NEQ -> true
    | _ -> false

  let is_associative = function
    | PLUS | TIMES | AND | OR | XOR -> true
    | _ -> false

  include PP
  let string_of_binop = asprintf "%a" pp_binop
end

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


  (** Arithmetic operations *)
  let ( + ) = binop plus
  let ( - ) = binop minus
  let ( * ) = binop times
  let ( / ) = binop divide
  let ( /$ ) = binop sdivide
  let ( mod ) = binop modulo
  let ( %$ ) = binop smodulo

  (** Bit operations *)
  let ( lsl ) = binop lshift
  let ( lsr ) = binop rshift
  let ( asr ) = binop arshift
  let ( land ) a b   = binop bit_and a b
  let ( lor  ) a b   = binop bit_or  a b
  let ( lxor ) a b   = binop bit_xor a b
  let lnot     a     = unop  not a

  (** Equality tests *)
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
