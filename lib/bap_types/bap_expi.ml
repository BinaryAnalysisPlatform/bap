open Core_kernel.Std
open Regular.Std
open Bap_common
open Bap_bil
open Bap_result

module Sz = Bap_size
module Monad = Bap_monad
module SM = Monad.State
module TE = Bap_type_error
open SM

class context = object(self)
  inherit Bap_context.t
  val id = Id.(succ zero)
  method private next = {< id = Id.succ id >}
  method create_undefined = self#next, undefined id
  method create_word u = self#next, word u id
  method create_storage s = self#next, storage s id
end

let var_t var = Bap_var.typ var
let imm_t imm = Type.Imm (Bitvector.bitwidth imm)
let bool_t = Type.Imm 1

let (^^) = Bitvector.concat
let succ = Bitvector.succ

let create_result f =
  SM.get () >>= fun ctxt ->
  let (ctxt,v) = f ctxt in
  SM.put ctxt >>= fun () ->
  SM.return v

let undefined () =
  create_result (fun ctxt -> ctxt#create_undefined)

let word w =
  create_result (fun ctxt -> ctxt#create_word w)

let storage s =
  create_result (fun ctxt -> ctxt#create_storage s)

let is_shift = function
  | Binop.LSHIFT | Binop.RSHIFT | Binop.ARSHIFT -> true
  | _ -> false

class ['a] t = object(self)
  constraint 'a = #context

  method private bot : 'a r = undefined ()

  method empty = new Storage.sparse
  method division_by_zero () : 'a r = self#bot
  method undefined_addr (a : word) : 'a r = self#bot
  method undefined_var v = self#bot

  method type_error (err : TE.t) = self#bot

  method lookup v : 'a r =
    get () >>= fun ctxt -> match ctxt#lookup v with
    | Some v -> return v
    | None -> self#undefined_var v

  method update var data : 'a u =
    get () >>= fun s -> put @@ s#update var data

  method eval_exp e : 'a r = match e with
    | Exp.Load (m,a,e,s) -> self#eval_load ~mem:m ~addr:a e s
    | Exp.Store (m,a,u,e,s) -> self#eval_store m a u e s
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

  method private eval e = self#eval_exp e >>| value

  method eval_int = word

  method eval_load ~mem ~addr endian sz =
    self#eval addr >>= function
    | Bot -> self#bot
    | Mem _ -> self#type_error TE.bad_mem
    | Imm addr -> self#eval mem >>= function
      | Bot -> self#bot
      | Imm _ -> self#type_error TE.bad_imm
      | Mem mem -> self#load_word mem addr endian (Sz.in_bits sz)

  method load mem addr = match mem#load addr with
    | None -> self#undefined_addr addr
    | Some w -> word w

  method private load_word mem addr ed size : 'a r  =
    let v = self#load mem addr in
    if size = 8 then v else
      self#load_word mem (succ addr) ed (size - 8) >>= fun u -> v >>=
      fun v ->
      let u = value u and v = value v in
      match ed with
      | LittleEndian -> self#eval_concat' u v
      | BigEndian    -> self#eval_concat' v u

  method eval_store ~mem ~addr word e s : 'a r =
    self#eval word >>= function
    | Bot -> self#bot
    | Mem mem -> self#type_error TE.bad_mem
    | Imm word -> self#eval addr >>= function
      | Bot -> self#bot
      | Mem mem -> self#type_error TE.bad_imm
      | Imm addr -> self#eval mem >>= function
        | Bot -> self#store_word self#empty addr word e (Sz.in_bits s)
        | Imm _ -> self#type_error TE.bad_mem
        | Mem mem -> self#store_word mem addr word e (Sz.in_bits s)

  method store mem addr word : 'a r = storage (mem#save addr word)

  method private store_word mem addr word ed sz =
    let hd_ct,tl_ct = match ed with
      | LittleEndian -> Cast.(LOW,HIGH)
      | BigEndian    -> Cast.(HIGH,LOW) in
    self#eval_cast' hd_ct 8 word |> function
    | None -> self#type_error `bad_cast
    | Some hd -> self#store mem addr hd >>| value >>= function
      | Bot -> self#bot
      | Imm _ -> self#type_error TE.bad_mem
      | Mem mem when sz = 8 -> storage mem
      | Mem mem -> self#eval_cast' tl_ct (sz - 8) word |> function
        | Some tl -> self#store_word mem (succ addr) tl ed (sz - 8)
        | None -> self#type_error `bad_cast

  method eval_var var = self#lookup var

  method eval_binop op u v =
    let open Bitvector in
    self#eval_exp u >>= fun u ->
    self#eval_exp v >>= fun v ->
    match value u, value v with
    | Bot,_ | _,Bot -> self#bot
    | Mem v,_ | _, Mem v -> self#type_error TE.bad_imm
    | Imm u, Imm v ->
      if is_shift op || Int.(bitwidth u = bitwidth v)
      then try word @@ match op with
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


  method eval_unop op u = self#eval u >>= function
    | Bot -> self#bot
    | Mem _ -> self#type_error TE.bad_imm
    | Imm u -> word @@ match op with
      | Unop.NEG -> Bitvector.(neg u)
      | Unop.NOT -> Bitvector.(lnot u)

  method eval_cast ct sz u = self#eval u >>= function
    | Bot -> self#bot
    | Mem v -> self#type_error TE.bad_imm
    | Imm u -> match self#eval_cast' ct sz u with
      | None -> self#type_error `bad_cast
      | Some r -> word r

  method private eval_cast' ct sz u : word option =
    let open Bitvector in
    try Option.return @@ match ct with
      | Cast.UNSIGNED -> extract_exn ~hi:Int.(sz - 1) u
      | Cast.SIGNED   -> extract_exn ~hi:Int.(sz - 1) (signed u)
      | Cast.HIGH     -> extract_exn ~lo:Int.(bitwidth u - sz) u
      | Cast.LOW      -> extract_exn ~hi:Int.(sz - 1) u
    with exn -> None

  method eval_let var u body =
    self#eval_exp u >>= fun u ->
    self#lookup var >>= fun w ->
    self#update var u >>= fun () ->
    self#eval_exp body >>= fun r ->
    self#update var w >>= fun () ->
    return r

  method eval_unknown _ _ = self#bot

  method eval_ite ~cond ~yes:t ~no:f =
    self#eval cond >>= function
    | Bot -> self#bot
    | Mem v -> self#type_error TE.bad_imm
    | Imm u ->
      if Bitvector.(u = b1)
      then self#eval_exp t
      else if Bitvector.(u = b0)
      then self#eval_exp f
      else self#type_error @@ TE.bad_type bool_t (imm_t u)

  method eval_extract hi lo w = self#eval w >>= function
    | Bot -> self#bot
    | Mem v -> self#type_error TE.bad_imm
    | Imm w ->
      try word (Bitvector.extract_exn ~hi ~lo w)
      with exn -> self#type_error `bad_cast

  method eval_concat u w : 'a r =
    self#eval_exp u >>= fun u ->
    self#eval_exp w >>= fun w ->
    self#eval_concat' (value u) (value w)

  method private eval_concat' u w : 'a r =
    match u,w with
    | Bot,_ | _,Bot -> self#bot
    | Mem v,_ | _,Mem v -> self#type_error TE.bad_imm
    | Imm u, Imm w -> word (Bitvector.concat u w)

end
