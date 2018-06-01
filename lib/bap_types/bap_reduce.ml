open Core_kernel
open Bap_common
open Bap_visitor
open Bap_stmt.Stmt
open Bap_bil
open Bap_bil.Stmt
open Bap_bil.Exp

module Word = Bitvector
module Var = Bap_var

type env = word Var.Map.t

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

let remove_defs env s =
  (object
    inherit [env] bil_visitor
    method! enter_move v _ env = Map.remove env v
  end)#run [s] env

(* requires: let-free *)
let reduce_consts bil =
  let rec run acc env = function
    | [] -> List.rev acc
    | Move (x,y) :: bil ->
      let y = y @@ env in
      run (move x y :: acc) (update env x y) bil
    | While _ as s :: bil ->
      let env = remove_defs env s in
      run (s :: acc) env bil
    | Jmp dst :: bil ->
      run (jmp (dst @@ env) :: acc) env bil
    | CpuExn _ | Special _ as s :: bil ->
      run (s :: acc) env bil
    | If (cond, yes, no) as s :: bil ->
      match cond @@ env with
      | Int w when Word.is_one w -> run acc env (yes @ bil)
      | Int w when Word.is_zero w -> run acc env (no @ bil)
      | _ ->
        let yes = run [] env yes in
        let no  = run [] env no  in
        let acc = if_ (cond @@ env) yes no :: acc in
        let env = remove_defs env s in
        run acc env bil in
  run [] empty_env bil
