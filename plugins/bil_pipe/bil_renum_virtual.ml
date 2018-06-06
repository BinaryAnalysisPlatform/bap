open Core_kernel
open Bap.Std

let numerator () =
  let num = ref 0 in
  fun typ ->
    let name = sprintf "v%d" !num in
    incr num;
    Var.create ~is_virtual:true name typ

let virtuals bil =
  let num = numerator () in
  let add_if_virtual v vs =
    if Map.mem vs v || Var.is_physical v then vs
    else Map.add vs v (num (Var.typ v)) in
  (object
    inherit [var Var.Map.t] Stmt.visitor as super

    method! enter_exp e vs = match e with
      | Let (v,x,y) ->
        add_if_virtual v vs |>
        super#enter_exp x |>
        super#enter_exp y
      | e -> super#enter_exp e vs

    method! enter_move v e vs =
      add_if_virtual v vs |>
      super#enter_exp e

  end)#run bil Var.Map.empty

class renumerator vars =
  let renum v = Option.value ~default:v (Map.find vars v) in
  object
    inherit Stmt.mapper as super

    method! map_let v ~exp ~body =
      Let (renum v, super#map_exp exp, super#map_exp body)

    method! map_var v = Var (renum v)

    method! map_stmt s = match s with
      | Move (v, e) -> [Move (renum v, super#map_exp e)]
      | s -> super#map_stmt s
  end

let renum bil =
  let vs = virtuals bil in
  Stmt.map (new renumerator vs) bil
