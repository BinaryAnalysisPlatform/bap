open Core_kernel
open Bap.Std
open Bil.Types
open Monads.Std

module Propagate(SM : Monad.State.S2) = struct

  open SM.Syntax

  type const = Def of word | Top
  type ctxt = const Var.Map.t
  type 'a r = ('a, ctxt) SM.t

  let new_ctxt = Var.Map.empty
  let find = Map.find
  let update = Map.add

  class e = object(self)

    method update const var : unit r =
      SM.get () >>= fun ctxt ->
      SM.put (update ctxt var const)

    method eval_var v : exp r =
      SM.get () >>= fun ctxt ->
      match find ctxt v with
      | Some (Def w) -> SM.return (Int w)
      | _ -> SM.return (Var v)

    method eval_load ~mem ~addr e s =
      self#eval_exp mem >>= fun mem ->
      self#eval_exp addr >>= fun addr ->
      SM.return (Bil.load mem addr e s)

    method eval_store ~mem ~addr data e s =
      self#eval_exp mem >>= fun mem ->
      self#eval_exp addr >>= fun addr ->
      self#eval_exp data >>= fun data ->
      SM.return (Bil.store mem addr data e s)

    method eval_unop op e =
      self#eval_exp e >>= fun e ->
      SM.return (Bil.unop op e)

    method eval_binop op x y =
      self#eval_exp x >>= fun x ->
      self#eval_exp y >>= fun y ->
      SM.return (Bil.binop op x y)

    method eval_int x = SM.return (Int x)

    method private update_var v e =
      match e with
      | Int w -> self#update (Def w) v
      | _ -> self#update Top v

    method eval_let v x y =
      SM.get () >>= fun ctxt ->
      self#eval_exp x >>= fun x ->
      self#update_var v x >>= fun () ->
      self#eval_exp y >>= fun y ->
      SM.put ctxt >>= fun () ->
      match y with
      | Int _ as e -> SM.return e
      | _ -> SM.return (Bil.let_ v x y)

    method eval_cast c s e =
      self#eval_exp e >>= fun e ->
      SM.return (Bil.cast c s e)

    method eval_ite ~cond ~yes ~no =
      self#eval_exp cond >>= fun cond ->
      self#eval_exp yes >>= fun yes ->
      self#eval_exp no  >>= fun no ->
      SM.return (Bil.ite cond yes no)

    method eval_extract hi lo e =
      self#eval_exp e >>= fun e ->
      SM.return (Bil.extract hi lo e)

    method eval_concat x y =
      self#eval_exp x >>= fun x ->
      self#eval_exp y >>= fun y ->
      SM.return (Bil.concat x y)

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
      | Unknown _ -> SM.return e
      | Ite (cond,yes,no) -> self#eval_ite ~cond ~yes ~no
      | Extract (hi,lo,w) -> self#eval_extract hi lo w
      | Concat (u,w) -> self#eval_concat u w

  end

  let meet =
    Map.merge ~f:(fun ~key:_ -> function
        | `Both (Def w1, Def w2) ->
          if Word.equal w1 w2 then Some (Def w1)
          else Some Top
        | _ -> Some Top)

  let defs bil =
    (object
      inherit [Var.Set.t] Stmt.visitor
      method! enter_move v _ defs = Set.add defs v
    end)#run bil Var.Set.empty |> Set.to_list

  let has_jmps bil =
    (object
      inherit [unit] Stmt.finder
      method! enter_jmp _ c = c.return (Some ())
    end)#find bil |> Option.is_some

  class t = object(self)
    inherit e

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
      match e with
      | Int w ->
        self#update (Def w) v >>= fun () ->
        SM.return (Bil.move v e)
      | e ->
        self#eval_exp e >>= fun e ->
        self#update Top v >>= fun () ->
        SM.return (Bil.move v e)

    method eval_if cond yes no =
      SM.get () >>= fun ctxt ->
      self#eval_exp cond >>= fun cond ->
      self#eval yes >>= fun yes ->
      SM.get () >>= fun ctxt_yes ->
      SM.put ctxt >>= fun () ->
      self#eval no >>= fun no ->
      SM.get () >>= fun ctxt_no ->
      let ctxt = match has_jmps yes, has_jmps no with
        | false, false -> meet ctxt_yes ctxt_no
        | true, true -> ctxt
        | false, _ -> ctxt_yes
        | _ -> ctxt_no in
      SM.put ctxt >>= fun () ->
      SM.return (Bil.if_ cond yes no)

    method eval_while cond body =
      SM.get () >>= fun ctxt ->
      SM.List.iter (defs body) ~f:(self#update Top) >>= fun () ->
      self#eval_exp cond >>= fun cond ->
      self#eval body >>= fun body ->
      SM.get () >>= fun ctxt' ->
      let ctxt = if has_jmps body then ctxt
          else meet ctxt ctxt' in
      SM.put ctxt >>= fun () ->
      SM.return (Bil.while_ cond body)

    method eval_jmp e : stmt r =
      self#eval_exp e >>= fun e ->
      SM.return (Bil.jmp e)
  end
end

module Dead_code = struct

  let is_used var bil =
    let (||) x y = match x, y with
      | Some r1, Some r2 -> Some (r1 || r2)
      | Some _, None | None, Some _ -> Some true
      | None, None -> None in
    let is_in_free s = Set.mem (Stmt.free_vars s) var in
    let defined = Var.equal var in
    let rec used_in = function
      | [] -> None
      | s :: bil -> match s with
        | If (_, yes, no) ->
          let r = used_in yes || used_in no in
          if Option.is_some r then r
          else used_in bil
        | While (_, body) -> used_in body || used_in bil
        | Move (var',_) when defined var' -> Some false
        | s when is_in_free s -> Some true
        | _ -> used_in bil in
    match used_in bil with
    | None -> false
    | Some x -> x

  let eliminate bil =
    let decr = function
      | [] -> []
      | _ :: scope -> scope in
    let rec loop acc scope = function
      | [] -> List.rev acc
      | st :: bil ->
        let scope = decr scope in
        match st with
        | Move (v, Int _) when Var.is_virtual v ->
	  if is_used v scope then loop (st :: acc) scope bil
	  else loop acc scope bil
        | If (cond, yes, no) ->
	  let yes = loop [] (yes @ scope) yes in
	  let no  = loop [] (no @ scope) no in
	  loop (If (cond, yes, no) :: acc) scope bil
        | While (cond,body) ->
          let body = loop [] (body @ bil) body in
          loop (While (cond,body) :: acc) scope bil
        | st -> loop (st :: acc) scope bil in
    loop [] bil bil

end

module P = Propagate(Monad.State)

let propagate_consts xs = Monad.State.eval ((new P.t)#eval xs) P.new_ctxt
let eliminate_dead_code = Dead_code.eliminate

let propagate_consts bil =
  let f bil =
    Bil.fold_consts bil |> propagate_consts |> eliminate_dead_code in
  Bil.fixpoint f bil
