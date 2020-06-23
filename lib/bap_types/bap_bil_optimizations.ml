open Core_kernel
open Monads.Std
open Bap_common
open Bap_bil
open Bap_visitor

open Bap_bil.Stmt
open Bap_stmt.Stmt

module Word = Bitvector
module Var = Bap_var

module Const = struct
  open Bap_bil.Exp

  type t =
    | Immediate of word
    | Undefined of string * typ
    | Top

  let equal x y = match x,y with
    | Top, Top -> true
    | Immediate w1, Immediate w2 -> Word.equal w1 w2
    | Undefined (s1,t1), Undefined (s2,t2) ->
      String.equal s1 s2 && Type.compare t1 t2 = 0
    | _ -> false

  let is_top = function | Top -> true | _ -> false
  let immediate w = Immediate w
  let undefined s t = Undefined (s,t)
  let top = Top

  let exp_of_const = function
    | Immediate w -> Some (Int w)
    | Undefined (s,t) -> Some (Unknown (s,t))
    | Top -> None

end


module Propagate(SM : Monad.State.S2) = struct
  open SM.Syntax

  open Bap_bil.Exp
  open Bap_exp.Exp
  open Bap_exp.Binop

  type const = Const.t
  type ctxt = const Var.Map.t
  type 'a r = ('a, ctxt) SM.t

  let new_ctxt = Var.Map.empty
  let find = Map.find
  let update = Map.set

  class exp_propagator = object(self)

    method update const v : unit r =
      SM.get () >>= fun ctxt ->
      SM.put (update ctxt v const)

    method private update_var v e =
      let const = match e with
        | Int w -> Const.immediate w
        | Unknown (s,t) -> Const.undefined s t
        | _ -> Const.top in
      self#update const v

    method eval_var v : exp r =
      SM.get () >>= fun ctxt ->
      match find ctxt v with
      | None -> SM.return (Var v)
      | Some c -> match Const.exp_of_const c with
        | None -> SM.return (Var v)
        | Some e -> SM.return e

    method eval_load ~mem ~addr e s =
      self#eval_exp mem  >>= fun mem ->
      self#eval_exp addr >>= fun addr ->
      SM.return (load mem addr e s)

    method eval_store ~mem ~addr data e s =
      self#eval_exp mem  >>= fun mem ->
      self#eval_exp addr >>= fun addr ->
      self#eval_exp data >>= fun data ->
      SM.return (store mem addr data e s)

    method eval_unop op e =
      self#eval_exp e >>= fun e ->
      SM.return (unop op e)

    method eval_binop op x y =
      self#eval_exp x >>= fun x ->
      self#eval_exp y >>= fun y ->
      SM.return (binop op x y)

    method eval_int x : exp r = SM.return (Int x)
    method eval_unknown x y : exp r = SM.return (Unknown (x,y))

    method eval_let v x y =
      SM.get () >>= fun ctxt ->
      self#eval_exp x >>= fun x ->
      self#update_var v x >>= fun () ->
      self#eval_exp y >>= fun y ->
      SM.put ctxt >>= fun () ->
      match y with
      | Int _ | Unknown _ as e -> SM.return e
      | _ -> SM.return (let_ v x y)

    method eval_cast c s e =
      self#eval_exp e >>= fun e ->
      SM.return (cast c s e)

    method eval_ite ~cond ~yes ~no =
      self#eval_exp cond >>= fun cond ->
      self#eval_exp yes >>= fun yes ->
      self#eval_exp no  >>= fun no ->
      SM.return (ite cond yes no)

    method eval_extract hi lo e =
      self#eval_exp e >>= fun e ->
      SM.return (extract hi lo e)

    method eval_concat x y =
      self#eval_exp x >>= fun x ->
      self#eval_exp y >>= fun y ->
      SM.return (concat x y)

    method eval_exp e =
      match e with
      | Load (m,a,e,s) -> self#eval_load ~mem:m ~addr:a e s
      | Store (m,a,u,e,s) -> self#eval_store ~mem:m ~addr:a u e s
      | Var v -> self#eval_var v
      | BinOp (op,u,v) -> self#eval_binop op u v
      | UnOp (op,u) -> self#eval_unop op u
      | Int u -> self#eval_int u
      | Cast (ct,sz,e) -> self#eval_cast ct sz e
      | Let (v,u,b) -> self#eval_let v u b
      | Unknown (x,y) -> self#eval_unknown x y
      | Ite (cond,yes,no) -> self#eval_ite ~cond ~yes ~no
      | Extract (hi,lo,w) -> self#eval_extract hi lo w
      | Concat (u,w) -> self#eval_concat u w
  end

  let join =
    Map.merge ~f:(fun ~key:_ -> function
        | `Both (c1,c2) ->
          if Const.equal c1 c2 then Some c1
          else Some Const.top
        | _ -> Some Const.top)

  let defs bil =
    (object
      inherit [Var.t list] bil_visitor
      method! enter_move v _ defs = v :: defs
    end)#run bil []

  let has_unconditional_jmps bil =
    List.exists bil
      ~f:(function | Jmp _ -> true | _ -> false)

  class bil_propagator = object(self)
    inherit exp_propagator

    method eval_stmt (s : stmt) : stmt r =
      match s with
      | Move (v,e)  -> self#eval_move v e
      | If (c,y,n)  -> self#eval_if c y n
      | While (c,b) -> self#eval_while c b
      | Jmp e -> self#eval_jmp e
      | _ -> SM.return s

    method eval (bil : bil) : bil r =
      SM.List.fold bil ~init:[] ~f:(fun acc s ->
          self#eval_stmt s >>= fun s ->
          SM.return (s :: acc)) >>= fun bil ->
      SM.return (List.rev bil)

    method eval_move v e : stmt r =
      self#eval_exp e >>= fun e ->
      self#update_var v e >>= fun () ->
      SM.return (move v e)

    method eval_if cond yes no =
      SM.get () >>= fun ctxt ->
      self#eval_exp cond >>= fun cond ->
      self#eval yes >>= fun yes ->
      SM.get () >>= fun ctxt_yes ->
      SM.put ctxt >>= fun () ->
      self#eval no >>= fun no ->
      SM.get () >>= fun ctxt_no ->
      let ctxt = match
          has_unconditional_jmps yes,
          has_unconditional_jmps no with
      | false, false -> join ctxt_yes ctxt_no
      | true, true -> ctxt
      | false, _ -> ctxt_yes
      | _ -> ctxt_no in
      SM.put ctxt >>= fun () ->
      SM.return (if_ cond yes no)

    method eval_while cond body =
      SM.get () >>= fun ctxt ->
      SM.List.iter (defs body) ~f:(self#update Top) >>= fun () ->
      self#eval_exp cond >>= fun cond ->
      self#eval body >>= fun body ->
      SM.get () >>= fun ctxt' ->
      let ctxt = if has_unconditional_jmps body then ctxt
        else join ctxt ctxt' in
      SM.put ctxt >>= fun () ->
      SM.return (while_ cond body)

    method eval_jmp e : stmt r =
      self#eval_exp e >>= fun e ->
      SM.return (jmp e)
  end
end

module P = Propagate(Monad.State)

let propagate_consts bil = Monad.State.eval ((new P.bil_propagator)#eval bil) P.new_ctxt

module Exp_helpers = Bap_helpers.Exp
module Stmt_helpers = Bap_helpers.Stmt

let prune_dead_virtuals bil =
  let (--) = Set.remove in
  let (++) = Set.union in
  let free = Exp_helpers.free_vars in
  let is_dead var live =
    Var.is_virtual var && not (Set.mem live var) in
  let rec loop init bil =
    List.fold (List.rev bil) ~init:([], init)
      ~f:(fun (bil, live) s -> match s with
          | Special _ | CpuExn _ -> s :: bil, live
          | Jmp e -> s :: bil, free e ++ live
          | If (cond, yes, no) ->
            let yes, live' = loop live yes in
            let no, live'' = loop live no in
            let live = free cond ++ live' ++ live'' in
            if_ cond yes no :: bil, live
          | Move (var,e) ->
            if is_dead var live then bil, live
            else s :: bil, free e ++ (live -- var)
          | While (cond, body) ->
            let live' = List.fold body ~init:live
                ~f:(fun live s -> live ++ Stmt_helpers.free_vars s) in
            let live = free cond ++ live' in
            s :: bil, live) in
  fst (loop Var.Set.empty bil)
