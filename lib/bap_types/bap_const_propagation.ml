open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor
open Stmt

module Var = Bap_var
module Word = Bitvector
module Exp_helpers = Bap_helpers.Exp

let inline bil =
  (object
    inherit bil_mapper

    method! map_if ~cond ~yes ~no =
      match cond with
      | Int w when Word.is_one  w -> yes
      | Int w when Word.is_zero w -> no
      | _ -> [If (cond, yes, no)]

    method! map_while ~cond body =
      match cond with
      | Int w when Word.is_zero w -> []
      | _ -> [While (cond, body)]

  end)#run bil

let is_assigned = Bap_helpers.is_assigned

(** [remove_forward_defs bil defs]
    for every [x := exp] in [bil], where [exp] depends on some
    [y1 .. yN] redefined later then [x],
    removes both [x] and [y1 .. yN] from defs *)
let remove_forward_defs bil defs =
  (object
    inherit [word Var.Map.t] bil_visitor
    method! enter_move var e defs =
      let redefined = Set.filter (Exp_helpers.free_vars e)
          ~f:(fun v -> is_assigned v succs) in
      if Set.is_empty redefined then defs
      else
        Set.fold ~init:(Map.remove defs var) redefined
          ~f:(fun defs v ->
              if is_assigned v succs then Map.remove defs v
              else defs)
  end)#run bil defs

let substitute defs e =
  let free = Exp_helpers.free_vars e in
  let mapper defs = object
    inherit bil_mapper as super
    method! map_var v =
      if Set.mem free v then
        match Map.find defs v with
        | Some x -> Int x
        | None -> Var v
      else Var v
  end in
  Exp_helpers.fold_consts @@ (mapper defs)#map_exp e

let propagate_consts bil =
  let remove_unequal env env' =
    Map.merge env env' ~f:(fun ~key:var -> function
        | `Left w -> Some w
        | `Both (w1, w2) when Word.equal w1 w2 -> Some w1
        | `Both (w1, w2) when not (Word.equal w1 w2) -> None
        | _ -> None) in
  let rec run ss env = function
    | [] -> List.rev ss, env
    | Move (v,e) :: bil ->
      let env, e = match substitute env e with
        | Int w as e -> Map.add env v w, e
        | e -> Map.remove env v, e in
      run (Move (v,e) :: ss) env bil
    | If (cond,yes,no) :: bil ->
      let yes,env_yes = run [] env yes in
      let no, env_no  = run [] env no  in
      let cond = substitute env cond in
      let env' = match cond with
        | Int w when Word.is_one  w -> env_yes
        | Int w when Word.is_zero w -> env_no
        | _ ->
          let env = remove_unequal env env_yes in
          remove_unequal env env_no in
      run (If (cond,yes,no) :: ss) env' bil
    | While (cond,body) :: bil ->
      let env = remove_forward_defs body env in
      let body, env' = run [] env body in
      let cond = substitute env cond in
      let env' = match cond with
        | Int w when Word.is_one w -> env'
        | _ -> remove_unequal env env' in
      run (While (cond,body) :: ss) env' bil
    | Jmp dst :: bil ->
      let dst = substitute env dst in
      run (Jmp dst :: ss) env bil
    | s :: bil -> run (s :: ss) env bil in
  inline @@ fst @@ run [] Var.Map.empty bil
