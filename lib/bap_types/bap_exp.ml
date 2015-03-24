open Core_kernel.Std
open Bap_common

open Bap_bil

type binop = exp -> exp -> exp
type unop = exp -> exp

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
end

module Unop = struct
  open Unop
  let neg = NEG
  let not = NOT
end

module Cast = struct
  open Cast
  let unsigned = UNSIGNED
  let signed = SIGNED
  let high = HIGH
  let low = LOW
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

module PP = struct
  open Format
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

  let rec pp fmt exp =
    let open Bap_bil.Exp in
    let is_imm = function
      | Var _ | Int _ -> true
      | _ -> false in
    let a e = format_of_string
        (if is_imm e then "%a" else "(%a)") in
    let pr s = fprintf fmt s in
    match exp with
    | Load (Var _ as mem, idx, edn, s) ->
      pr "%a[%a, %a]:%a" pp mem pp idx pp_edn edn Bap_size.pp s
    | Load (mem, idx, edn, s) ->
      pr "(%a)[%a, %a]:%a" pp mem pp idx pp_edn edn Bap_size.pp s
    | Store (mem, idx, exp, edn, s) ->
      pr "@[<v2>%a@;with [%a, %a]:%a <- %a@]"
        pp mem pp idx pp_edn edn Bap_size.pp s pp exp
    | Ite (ce, te, fe) ->
      pr "@[<v2>if %a@;then %a@;else %a@]" pp ce pp te pp fe
    | Extract (hi, lo, exp) ->
      pr "extract: %d:%d[%a]" hi lo pp exp
    | Concat (le, re) ->
      pr (a le ^^ "." ^^ a re) pp le pp re
    | BinOp (op, le, re) ->
      pr (a le ^^ " %a " ^^ a re) pp le pp_binop op pp re
    | UnOp (op, exp) ->
      pr ("%a" ^^ a exp) pp_unop op pp exp
    | Var var -> Bap_var.pp fmt var
    | Int bv  -> Bap_bitvector.pp fmt bv
    | Cast (ct, n, exp) ->
      pr "%a:%d[%a]" pp_cast ct n pp exp
    | Let (var, def, body) ->
      pr "let %a = %a in@ %a" Bap_var.pp var pp def pp body
    | Unknown (s, typ) ->
      pr "unknown[%s]:%a" s Bap_type.pp typ
end

include Regular.Make(struct
    type t = Bap_bil.exp with bin_io, compare, sexp
    let hash = Hashtbl.hash
    let module_name = "Bap_exp"
    let pp = PP.pp
  end)
