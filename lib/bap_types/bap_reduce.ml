open Core_kernel
open Bap_common
open Bap_visitor
open Bap_stmt.Stmt
open Bap_bil.Stmt
open Bap_bil.Exp

module Word = Bitvector
module Var = Bap_var

let empty_env = Var.Map.empty

let apply e env = (object
  inherit bil_mapper as super
  method! map_var v =
    match Map.find env v with
    | None -> Var v
    | Some i -> Int i
end)#map_exp e |> Bap_helpers.Exp.fold_consts

let (@@) = apply

let update env var value =
  match value with
  | Int i -> Map.add env var i
  | _ -> Map.remove env var

let remove_bound_vars env s =
  (object
    inherit [Var.Set.t] bil_visitor
    method! enter_move v _ vs = Set.add vs v
  end)#run [s] Var.Set.empty |>
  Set.fold ~init:env ~f:Map.remove

(* requires: let-free *)
let reduce_consts bil =
  let rec run acc env = function
    | [] -> List.rev acc
    | Move (x,y) :: worklist ->
      let y = y @@ env in
      run (move x y :: acc) (update env x y) worklist
    | While _ as s :: worklist ->
      let env = remove_bound_vars env s in
      run (s :: acc) env worklist
    | Jmp dst :: worklist ->
      run (jmp (dst @@ env) :: acc) env worklist
    | CpuExn _ | Special _ as s :: worklist ->
      run (s :: acc) env worklist
    | If (cond, yes, no) as s :: worklist ->
      match cond @@ env with
      | Int w when Word.is_one w -> run acc env (yes @ worklist)
      | Int w when Word.is_zero w -> run acc env (no @ worklist)
      | _ ->
        let yes = run [] env yes in
        let no  = run [] env no  in
        let acc = if_ (cond @@ env) yes no :: acc in
        let env = remove_bound_vars env s in
        run acc env worklist in
  run [] empty_env bil
