open Core_kernel.Std
open Bap_common

module Ops = struct
  open Bap_bil

  open Bap_bil.Exp
  open Binop
  open Unop

  type binop = exp -> exp -> exp
  type unop = exp -> exp

  (** Arithmetic operations *)
  let ( + ) = binop plus
  let ( - ) = binop minus
  let ( * ) = binop times
  let ( / ) = binop divide
  let ( /$ ) = binop sdivide
  let ( mod ) = binop (mod)
  let ( %$ ) = binop smod

  (** Bit operations *)
  let ( lsl ) = binop lshift
  let ( lsr ) = binop rshift
  let ( asr ) = binop arshift
  let ( land ) a b   = binop AND a b
  let ( lor  ) a b   = binop OR  a b
  let ( lxor ) a b   = binop XOR a b
  let lnot     a     = unop  NOT a

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
          | LSHIFT  -> "lsl"
          | RSHIFT  -> "lsr"
          | ARSHIFT -> "asr"
          | AND     -> "land"
          | OR      -> "lor"
          | XOR     -> "lxor"
          | EQ      -> "="
          | NEQ     -> "<>"
          | LT      -> "<"
          | LE      -> "<="
          | SLT     -> "<$"
          | SLE     -> "<=$")

  let pp_unop fmt op = fprintf fmt "%s"
      Unop.(match op with
          | NEG -> "-"
          | NOT -> "lnot")

  let pp_edn fmt e = fprintf fmt "%s"
      Bap_bil.(match e with
          | LittleEndian -> "el"
          | BigEndian    -> "be")

  let rec pp fmt exp =
    let open Bap_bil.Exp in match exp with
    | Load (mem, idx, edn, s) ->
      fprintf fmt "%a[%a, %a]:%a" pp mem pp idx pp_edn edn Bap_size.pp s
    | Store (mem, idx, exp, edn, s) ->
      fprintf fmt "@[<v2>%a with@;[%a, %a]:%a <- %a@]"
        pp mem pp idx pp_edn edn Bap_size.pp s pp exp
    | Ite (ce, te, fe) ->
      fprintf fmt "@[<v2>if %a@;then %a@;else %a@]" pp ce pp te pp fe
    | Extract (hi, lo, exp) ->
      fprintf fmt "extract: %d:%d[%a]" hi lo pp exp
    | Concat (le, re) ->
      fprintf fmt "(%a)^(%a)" pp le pp re
    | BinOp (op, le, re) ->
      fprintf fmt "(%a) %a (%a)" pp le pp_binop op pp re
    | UnOp (op, exp) ->
      fprintf fmt "%a(%a)" pp_unop op pp exp
    | Var var -> Bap_var.pp fmt var
    | Int bv  -> Bap_bitvector.pp fmt bv
    | Cast (ct, n, exp) ->
      fprintf fmt "%a:%d[%a]" pp_cast ct n pp exp
    | Let (var, def, body) ->
      fprintf fmt "let %a = %a in %a" Bap_var.pp var pp def pp body
    | Unknown (s, typ) ->
      fprintf fmt "unknown[%s]:%a" s Bap_type.pp typ
end

include Regular.Make(struct
    include Bap_bil.Exp
    let hash = Hashtbl.hash
    let module_name = "Bap_exp"
    let pp = PP.pp
  end)

include Ops
