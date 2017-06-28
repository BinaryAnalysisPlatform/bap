open Core_kernel.Std
open Monads.Std
open Regular.Std
open Bap_common
open Bap_bil
open Bap_eval_types

module type S = Eval.S
module type S2 = Eval.S2
module Sz = Bap_size
module TE = Bap_type_error

type word = Bitvector.t
type endian = Bitvector.endian

let var_t var = Bap_var.typ var
let imm_t imm = Type.Imm (Bitvector.bitwidth imm)
let bool_t = Type.Imm 1
let (^^) = Bitvector.concat
let succ = Bitvector.succ

let reduce_let exp =
  let open Exp in
  let rec (/) d x = match x with
    | Load (x,y,e,s) -> Load (d/x, d/y, e, s)
    | Store (x,y,z,e,s) -> Store (d/x, d/y, d/z, e, s)
    | BinOp (b,x,y) -> BinOp (b, d/x, d/y)
    | UnOp (o,x) -> UnOp (o, d/x)
    | Int _ | Unknown _ as x -> x
    | Cast (c,n,x) -> Cast (c,n,d/x)
    | Ite (x,y,z) -> Ite (d/x, d/y, d/z)
    | Extract (n,m,x) -> Extract (n,m, d/x)
    | Concat (x,y) -> Concat (d/x, d/y)
    | Let (v,x,y) -> ((v,d/x)::d)/y
    | Var x as var -> match List.Assoc.find d x with
      | None -> var
      | Some exp -> exp in
  []/exp

let is_shift = function
  | Binop.LSHIFT | Binop.RSHIFT | Binop.ARSHIFT -> true
  | _ -> false

module Make2(State : Monad.S2) = struct
  open State.Syntax

  module M = State

  type ('a,'e) state = ('a,'e) M.t
  type ('a,'e) m = ('a,'e) state
  class type ['a,'r] semantics = ['a,'r] Eval.T2(State).semantics
  class type virtual ['a,'r,'s] domain  = ['a,'r,'s] Eval.T2(State).domain
  class type virtual ['a,'r,'s] eff = ['a,'r,'s] Eval.T2(State).eff

  class type virtual ['a,'r,'s] eval = object
    inherit ['a,'r,'s] domain
    inherit ['a,'r,'s] eff
    inherit ['a,'r] semantics
    method type_error : TE.type_error -> ('r,'a) state
    method division_by_zero : unit -> ('r,'a) state
  end


  class virtual ['a,'r,'s] t : ['a,'r,'s] eval = object(self)
    method virtual lookup : var -> ('r,'a) m
    method virtual update : var -> 'r -> (unit,'a) m
    method virtual load  : 's -> addr -> ('r,'a) m
    method virtual store : 's -> addr -> word -> ('r,'a) m

    method private virtual undefined : ('r,'a) m
    method private virtual value_of_word : word -> ('r,'a) m
    method private virtual word_of_value : 'r -> (word option,'a) m
    method private virtual storage_of_value : 'r -> ('s option,'a) m

    method private bot = self#undefined

    method division_by_zero () = self#bot

    method type_error (err : TE.t) = self#bot

    method eval_exp : exp -> ('r,'a) m = function
      | Exp.Load (m,a,e,s) -> self#eval_load ~mem:m ~addr:a e s
      | Exp.Store (m,a,u,e,s) -> self#eval_store ~mem:m ~addr:a u e s
      | Exp.Var v -> self#eval_var v
      | Exp.BinOp (op,u,v) -> self#eval_binop op u v
      | Exp.UnOp (op,u) -> self#eval_unop op u
      | Exp.Int u -> self#eval_int u
      | Exp.Cast (ct,sz,e) -> self#eval_cast ct sz e
      | Exp.Let (v,u,b) -> self#eval_let v u b
      | Exp.Unknown (m,t) -> self#eval_unknown m t
      | Exp.Ite (cond,yes,no) -> self#eval_ite ~cond ~yes ~no
      | Exp.Extract (hi,lo,w) -> self#eval_extract hi lo w
      | Exp.Concat (u,w) -> self#eval_concat u w

    method eval_int : word -> ('r,'a) m = self#value_of_word

    method private eval_imm exp : (word option,'a) m =
      self#eval_exp exp >>= self#word_of_value

    method private eval_mem exp : ('s option,'a) m =
      self#eval_exp exp >>= self#storage_of_value

    method eval_load ~mem ~addr endian sz =
      self#eval_mem mem >>= function
      | None -> self#type_error TE.bad_mem
      | Some mem ->
        self#eval_imm addr >>= function
        | None -> self#type_error TE.bad_imm
        | Some addr ->
          self#load_word mem addr endian (Sz.in_bits sz)


    method private load_word mem addr ed size =
      let v = self#load mem addr in
      if size = 8 then v else
        self#load_word mem (succ addr) ed (size - 8) >>= fun u ->
        v >>= fun v ->
        self#word_of_value u >>= fun u ->
        self#word_of_value v >>= fun v ->
        match u,v with
        | None,_|_,None -> self#type_error TE.bad_imm
        | Some u, Some v -> match ed with
          | LittleEndian -> self#eval_concat' u v
          | BigEndian    -> self#eval_concat' v u

    method eval_store ~mem ~addr word (e : endian) s =
      self#eval_mem  mem >>= function
      | None -> self#type_error TE.bad_mem
      | Some mem ->
        self#eval_imm addr >>= function
        | None -> self#type_error TE.bad_imm
        | Some addr ->
          self#eval_imm word >>= function
          | None -> self#type_error TE.bad_imm
          | Some word ->
            self#store_word mem addr word e (Sz.in_bits s)

    method private store_word mem addr word (ed : Bitvector.endian) sz =
      let hd_ct,tl_ct = match ed with
        | LittleEndian -> Cast.(LOW,HIGH)
        | BigEndian    -> Cast.(HIGH,LOW) in
      self#eval_cast' hd_ct 8 word |> function
      | None -> self#type_error `bad_cast
      | Some hd -> self#store mem addr hd >>= fun mem ->
        if sz = 8 then State.return mem
        else self#storage_of_value mem >>= function
          | None -> self#type_error TE.bad_mem
          | Some mem ->
            self#eval_cast' tl_ct (sz - 8) word |> function
            | Some tl -> self#store_word mem  (succ addr) tl ed (sz - 8)
            | None -> self#type_error `bad_cast

    method eval_var var = self#lookup var

    method eval_binop op u v =
      let open Bitvector in
      self#eval_imm u >>= function
      | None -> self#type_error TE.bad_imm
      | Some u ->
        self#eval_imm v >>= function
        | None -> self#type_error TE.bad_imm
        | Some v ->
          if is_shift op || Int.(bitwidth u = bitwidth v)
          then try self#value_of_word @@ match op with
            | Binop.PLUS -> u + v
            | Binop.MINUS -> u - v
            | Binop.TIMES -> u * v
            | Binop.DIVIDE -> u / v
            | Binop.SDIVIDE -> signed u / signed v
            | Binop.MOD -> u mod v
            | Binop.SMOD -> signed u mod signed v
            | Binop.LSHIFT -> u lsl v
            | Binop.RSHIFT -> u lsr v
            | Binop.ARSHIFT -> u asr v
            | Binop.AND -> u land v
            | Binop.OR -> u lor v
            | Binop.XOR -> u lxor v
            | Binop.EQ -> Bitvector.(of_bool (u = v))
            | Binop.NEQ -> Bitvector.(of_bool (u <> v))
            | Binop.LT -> Bitvector.(of_bool (u < v))
            | Binop.LE -> Bitvector.(of_bool (u <= v))
            | Binop.SLT -> Bitvector.(of_bool (signed u < signed v))
            | Binop.SLE  -> Bitvector.(of_bool (signed u <= signed v))
            with Division_by_zero -> self#division_by_zero ()
          else self#type_error @@ TE.bad_type (imm_t u) (imm_t v)

    method eval_unop op u =
      self#eval_imm u >>= function
      | None -> self#type_error TE.bad_imm
      | Some u -> self#value_of_word @@ match op with
        | Unop.NEG -> Bitvector.(neg u)
        | Unop.NOT -> Bitvector.(lnot u)

    method eval_cast ct sz u =
      self#eval_imm u >>= function
      | None -> self#type_error TE.bad_imm
      | Some u ->  match self#eval_cast' ct sz u with
        | None -> self#type_error `bad_cast
        | Some r -> self#value_of_word r

    method private eval_cast' ct sz u : word option =
      let open Bitvector in
      try Option.return @@ match ct with
        | Cast.UNSIGNED -> extract_exn ~hi:Int.(sz - 1) u
        | Cast.SIGNED   -> extract_exn ~hi:Int.(sz - 1) (signed u)
        | Cast.HIGH     -> extract_exn ~lo:Int.(bitwidth u - sz) u
        | Cast.LOW      -> extract_exn ~hi:Int.(sz - 1) u
      with exn -> None

    method eval_let var u body =
      self#eval_exp (reduce_let Exp.(Let (var,u,body)))

    method eval_unknown _ _ = self#bot

    method eval_ite ~cond ~yes:t ~no:f =
      self#eval_imm cond >>= function
      | None -> self#type_error TE.bad_imm
      | Some u ->
        if Bitvector.(u = b1)
        then self#eval_exp t
        else if Bitvector.(u = b0)
        then self#eval_exp f
        else self#type_error @@ TE.bad_type bool_t (imm_t u)

    method eval_extract hi lo w =
      self#eval_imm w >>= function
      | None -> self#type_error TE.bad_imm
      | Some w ->
        try self#value_of_word (Bitvector.extract_exn ~hi ~lo w)
        with exn -> self#type_error `bad_cast

    method eval_concat u w =
      self#eval_imm u >>= fun u ->
      self#eval_imm w >>= fun w ->
      match u,w with
      | Some u, Some w -> self#eval_concat' u w
      | _ -> self#type_error TE.bad_imm

    method private eval_concat' (u : word) (w : word) =
      self#value_of_word (Bitvector.concat u w)

  end
end

module Make(M : Monad.S) = struct
  type 'a m = 'a M.t
  module M = M
  module Eval2 = Make2(struct
      type ('a,'e) t = 'a M.t
      include (M : Monad.S with type 'a t := 'a M.t)
    end)

  class type ['r] semantics = ['r] Eval.T1(M).semantics
  class type virtual ['r,'s] domain  = ['r,'s] Eval.T1(M).domain
  class type virtual ['r,'s] eff = ['r,'s] Eval.T1(M).eff

  class type virtual ['r,'s] eval = object
    inherit ['r,'s] domain
    inherit ['r,'s] eff
    inherit ['r] semantics
    method type_error : TE.type_error -> 'r m
    method division_by_zero : unit -> 'r m
  end

  class virtual ['r,'s] t : ['r,'s] eval = ['a,'r,'s] Eval2.t
end

include Make2(Monad.State)
