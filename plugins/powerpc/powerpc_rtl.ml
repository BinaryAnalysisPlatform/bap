open Core_kernel
open Bap.Std

open Powerpc_utils

type sign = Signed | Unsigned [@@deriving bin_io, compare, sexp]

type binop = Bil.binop [@@deriving bin_io, compare, sexp]
type unop  = Bil.unop  [@@deriving bin_io, compare, sexp]

type body =
  | Vars of var * var list
  | Word of word
  | Load of (var * body * endian * size)
  | Concat of body * body
  | Binop of binop * body * body
  | Extract of (int * int * body)
  | Cast of (sign * int * body)
  | Unop of (unop * body)
[@@deriving bin_io, compare, sexp]

type exp = {
  body : body;
  sign : sign;
  width : int;
} [@@deriving bin_io, compare, sexp]

type t =
  | Move of exp * exp
  | Jmp of exp
  | Store of var * exp * exp * endian * size
  | If of exp * t list * t list
  | Foreach of bool * exp * exp * t list
  | Message of string
[@@deriving bin_io, compare, sexp]

type rtl = t [@@deriving bin_io, compare, sexp]

let store mem addr x endian size = Store (mem, addr, x, endian, size)
let jmp addr = Jmp addr
let move x y = Move (x,y)
let if_ cond then_ else_ = If (cond, then_, else_)
let foreach ~inverse step exp code = Foreach (inverse,step,exp,code)
let message m = Message m

let rec bil_exp = function
  | Vars (v, []) -> Bil.var v
  | Vars (v, vars) ->
    List.fold vars ~init:(Bil.var v) ~f:(fun e v -> Bil.(e ^ var v))
  | Word w -> Bil.int w
  | Load (mem, addr, endian, size) ->
    Bil.(load (var mem) (bil_exp addr) endian size)
  | Concat (x, y) -> Bil.(bil_exp x ^ bil_exp y)
  | Binop (op, x, y) -> Bil.binop op (bil_exp x) (bil_exp y)
  | Extract (hi, lo, x) -> Bil.extract hi lo (bil_exp x)
  | Cast (Signed, width, x) -> Bil.(cast signed width (bil_exp x))
  | Cast (Unsigned, width, x) -> Bil.(cast unsigned width (bil_exp x))
  | Unop (op, x) -> Bil.unop op (bil_exp x)

let var_bitwidth v =
  match Var.typ v with
  | Type.Imm w -> w
  | _ ->
    ppc_fail "variable %s doesn't has notion of bitwidth" (Var.name v)

let width_of_vars vs =
  List.fold ~init:0 ~f:(fun x v -> x + var_bitwidth v) vs

let var_of_exp e = match e.body with
  | Vars (v,_) -> v
  | _ -> ppc_fail "variable expected"

module Exp = struct

  let cast x width sign =
    let nothing_to_cast =
      (x.sign = sign && x.width = width) ||
      (x.width = width && width = 1) in
    if nothing_to_cast then x
    else
    if x.width = 1 then
      {width; sign; body = Cast (x.sign, width, x.body)}
    else
      {width; sign; body = Cast (sign, width, x.body)}

  let cast_width x width = cast x width x.sign

  let derive_sign s s' = match s, s' with
    | Signed, _ | _, Signed -> Signed
    | _ -> Unsigned

  let unop op x = { x with body = Unop (op, x.body)}

  let binop_with_signedness sign op lhs rhs =
    let width = max lhs.width rhs.width in
    let lhs = cast lhs width sign in
    let rhs = cast rhs width sign in
    let body = Binop(op, lhs.body, rhs.body) in
    {sign; width; body;}

  let unsigned_binop op lhs rhs =
    binop_with_signedness Unsigned op lhs rhs

  let signed_binop op lhs rhs =
    binop_with_signedness Signed op lhs rhs

  let binop_with_cast op lhs rhs =
    let sign = derive_sign lhs.sign rhs.sign in
    binop_with_signedness sign op lhs rhs

  let concat lhs rhs =
    let width = lhs.width + rhs.width in
    let body = Concat (lhs.body, rhs.body) in
    { sign = Unsigned; width; body; }

  let bit_result x = cast_width x 1

  let derive_op x y op_u op_s =
    match derive_sign x.sign y.sign with
    | Signed -> op_s
    | Unsigned -> op_u

  let plus  = binop_with_cast Bil.plus
  let minus = binop_with_cast Bil.minus
  let times = binop_with_cast Bil.times

  let lt x y  =
    let op = derive_op x y Bil.lt Bil.slt in
    bit_result (binop_with_cast op x y)

  let gt x y  =
    let op = derive_op x y Bil.lt Bil.slt in
    bit_result (binop_with_cast op y x)

  let le x y  =
    let op = derive_op x y Bil.le Bil.sle in
    bit_result (binop_with_cast op x y)

  let ge x y  =
    let op = derive_op x y Bil.le Bil.sle in
    bit_result (binop_with_cast op y x)

  let divide x y  =
    let op = derive_op x y Bil.divide Bil.sdivide in
    binop_with_cast op x y

  let modulo x y =
    let op = derive_op x y Bil.modulo Bil.smodulo in
    binop_with_cast op x y

  let eq x y  = bit_result (binop_with_cast Bil.eq x y)
  let neq x y = bit_result (binop_with_cast Bil.neq x y)

  let lshift = binop_with_cast Bil.lshift
  let rshift x y =
    let op =
      if x.sign = Signed then Bil.arshift
      else Bil.rshift in
    binop_with_cast op x y

  let bit_and = unsigned_binop Bil.bit_and
  let bit_xor = unsigned_binop Bil.bit_xor
  let bit_or  = unsigned_binop Bil.bit_or
  let not x = unop Bil.not x

  let of_var var =
    let width = var_bitwidth var in
    { sign = Unsigned; width; body = Vars (var, []); }

  let of_vars vars = match vars with
    | [] -> ppc_fail "can't constuct an expression from empty var list"
    | v :: vars ->
      let width = width_of_vars (v::vars) in
      { sign = Unsigned; width; body = Vars (v, vars); }

  let of_word w =
    let width = Word.bitwidth w in
    {sign = Unsigned; width; body = Word w }

  let tmp width =
    let var = Var.create ~is_virtual:true ~fresh:true "tmp" (Type.imm width) in
    of_var var

  let load mem addr endian size =
    let width = Size.in_bits size in
    let sign = Unsigned in
    let body = Load (mem,addr.body,endian,size) in
    {sign; width; body;}

  let extract_of_vars e hi lo vars =
    let width = hi - lo + 1 in
    let bounds,_ =
      List.fold ~init:([],0)
        ~f:(fun (acc,n) v ->
            let len = var_bitwidth v in
            let hi = e.width - n - 1 in
            let lo = e.width - n - len in
            (hi, lo, v) :: acc, n + len) vars in
    let bounds = List.rev bounds in
    let has_hi = List.exists ~f:(fun (hi',_,_) -> hi = hi') bounds in
    let has_lo = List.exists ~f:(fun (_,lo',_) -> lo = lo') bounds in
    if has_hi && has_lo then
      let vars = List.filter_map ~f:(fun (hi', lo', v) ->
          if hi' <= hi && lo' >= lo then Some v
          else None) bounds in
      let v = List.hd_exn vars in
      let vars = List.tl_exn vars in
      {sign=Unsigned; width; body = Vars (v,vars)}
    else
      {sign=Unsigned; width; body = Extract (hi,lo,e.body)}

  (* if target width is the same as original expression width, we
     return unsigned original, dependless what sign it had,
     because there is an invariant that extract always returns
     unsigned expression. *)
  let extract hi lo e =
    let width = hi - lo + 1 in
    if width = e.width then { e with sign=Unsigned; }
    else
      match e.body with
      | Vars (v,vars) when vars <> [] ->
        extract_of_vars e hi lo (v :: vars)
      | _ ->
        { sign=Unsigned; width; body = Extract (hi,lo,e.body) }

  let signed e = {e with sign = Signed}
  let unsigned e = {e with sign = Unsigned}
  let width e = e.width
  let body e = e.body
  let sign e = e.sign
  let bil_exp e = bil_exp e.body
end

module Infix = struct
  open Exp
  let ( := )  = move
  let ( + )  = plus
  let ( - )  = minus
  let ( * )  = times
  let ( / )  = divide
  let ( ^ )  = concat
  let ( % )  = modulo
  let ( < )  = lt
  let ( > )  = gt
  let ( <= )  = le
  let ( >= )  = ge
  let ( = )   = eq
  let ( <> )   = neq
  let ( << )  = lshift
  let ( >> )  = rshift
  let ( lor )  = bit_or
  let ( land ) = bit_and
  let ( lxor ) = bit_xor
  let ( lnot ) = not
end

module Translate = struct

  let store mem addr data endian size =
    let addr = bil_exp addr.body in
    let data = bil_exp data.body in
    Bil.[mem := store (var mem) addr data endian size]

  let if_ probe then_ else_ =
    let probe = bil_exp (Exp.body probe) in
    Bil.[ if_ probe then_ else_ ]

  let rec expand_vars e = match e with
    | Vars (v, vs) -> v :: vs
    | Concat (x,y) ->  expand_vars x @ expand_vars y
    | Cast _ | Extract _ | Load _ | Word _ | Binop _ | Unop _ -> []

  let partial_assign v (hi_var, lo_var) rhs (hi_exp, lo_exp) =
    let width = var_bitwidth v in
    let rhs_width = Exp.width rhs in
    let rhs =
      if hi_exp - lo_exp + 1 = rhs_width then rhs
      else Exp.extract hi_exp lo_exp rhs in
    let var = Bil.var v in
    let width_left = width - hi_var - 1 in
    let width_right = lo_var in
    let left =
      if width_left = 0 then None
      else Some (Bil.extract (width - 1) (hi_var + 1) var) in
    let middle = bil_exp (Exp.body rhs) in
    let right =
      if width_right = 0 then None
      else Some (Bil.extract (lo_var - 1) 0 var) in
    match left,right with
    | None, None -> Bil.[v := middle]
    | Some left, None -> Bil.[v := left ^ middle]
    | None, Some right -> Bil.[v := middle ^ right]
    | Some left, Some right -> Bil.[ v := left ^ middle ^ right; ]

  let assign_vars vars ?hi ?lo rhs =
    let in_bounds x left right = x <= left && x >= right in
    let vars = List.rev vars in
    let full = width_of_vars vars in
    let hi = Option.value ~default:(full - 1) hi in
    let lo = Option.value ~default:0 lo in
    let dif = hi - lo + 1 in
    let rhs = Exp.cast rhs dif (Exp.sign rhs) in
    let rhs_width = Exp.width rhs in
    let rec assign es assigned vars_lo = function
      | [] -> es
      | v :: vars ->
        let width = var_bitwidth v in
        let vars_hi = width + vars_lo - 1 in
        let var_in_bounds =
          hi < width
          || in_bounds vars_lo hi lo
          || in_bounds vars_hi hi lo in
        if var_in_bounds then
          let lo_var = max lo vars_lo  - vars_lo in
          let hi_var = min (rhs_width + lo - 1) vars_hi - vars_lo in
          let bits_to_assign = hi_var - lo_var + 1 in
          let hi_exp = assigned + bits_to_assign - 1 in
          let lo_exp = assigned in
          let es = partial_assign v (hi_var, lo_var) rhs (hi_exp,lo_exp) @ es in
          assign es (assigned + bits_to_assign) (vars_lo + width) vars
        else assign es assigned (vars_lo + width) vars in
    assign [] 0 0 vars

  (** valid forms of assignment:
      1) var := exp
      2) var1 ^ var2 ... varN := exp
      3) [var1;var2; ...] := exp  - equivalent to 2)
      4) extract hi lo var := exp - change only certain bits of var
      5) extract hi lo (var1 ^ var2 ... varN) := exp *)
  let rec move lhs rhs =
    match Exp.body lhs with
    | Vars (v, []) ->
      let rhs = Exp.(cast rhs (width lhs) (sign lhs)) in
      Bil.[v := bil_exp (Exp.body rhs)]
    | Vars (v, vars) -> assign_vars (v::vars) rhs
    | Concat (x,y) ->
      let vars = expand_vars x @ expand_vars y in
      let width = width_of_vars vars in
      let rhs = Exp.cast rhs width (Exp.sign rhs) in
      assign_vars vars rhs
    | Extract (hi, lo, x) ->
      let vars = expand_vars x in
      assign_vars vars ~hi ~lo rhs
    | _ -> ppc_fail "unexpected left side of :="

  let jmp exp = Bil.[ jmp (bil_exp exp.body)]

  class move_finder var =
    object inherit [unit] Stmt.finder
      method! enter_move v _ r =
        if Var.equal var v then r.return (Some ())
        else r
    end

  let rec stmt_to_bil = function
    | Move (x,y) -> move x y
    | Jmp a -> jmp a
    | If (cond, then_, else_) -> if_ cond (to_bil then_) (to_bil else_)
    | Message m -> [Bil.special m]
    | Store (mem, addr, data, endian, size) ->
      let bits = Size.in_bits size in
      let data =
        if bits = Exp.width data then data
        else Exp.extract (bits - 1) 0 data in
      store mem addr data endian size
    | Foreach (inverse,step_e, e, code) ->
      let iters = Exp.width e / Exp.width step_e in
      let stepw = Exp.width step_e in
      let has_assignments = has_assignments (var_of_exp step_e) code in
      to_bil @@ List.concat
        (List.init iters
           ~f:(fun i ->
               let i = if inverse then iters - i - 1 else i in
               let hi = (i + 1) * stepw - 1 in
               let lo = i * stepw in
               if has_assignments then
                 let last = Infix.(Exp.extract hi lo e := step_e) in
                 Infix.(step_e := Exp.extract hi lo e) :: code @ [last]
               else
                 Infix.(step_e := Exp.extract hi lo e) :: code))
  and to_bil ts =
    List.concat (List.map ~f:stmt_to_bil ts)
  and has_assignments var rtl =
    let bil = to_bil rtl in
    Option.is_some ((new move_finder var)#find bil)
end

let bil_of_t = Translate.to_bil
