open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor
open Stmt

module Var = Bap_var
module Word = Bitvector
module Exp_helpers = Bap_helpers.Exp

let is_assigned = Bap_helpers.is_assigned

(** [remove_forward_defs bil defs]
    for every [x := exp] in [bil], where [exp] depends on some
    [y1 .. yN] redefined later then [x],
    removes both [x] and [y1 .. yN] from defs *)
let remove_forward_defs bil defs =
  (object
    inherit [exp Var.Map.t] bil_visitor
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
  let mapper defs = object
    inherit bil_mapper as super
    method! map_var v = match Map.find defs v with
      | Some e -> e
      | None -> Var v
  end in
  Exp_helpers.fold_consts @@ (mapper defs)#map_exp e

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

let propagate_consts bil =
  let remove_unequal defs defs' =
    let diff = Map.symmetric_diff defs defs' ~data_equal:Bap_exp.equal in
    Sequence.fold diff ~init:defs ~f:(fun defs (v,diff) -> match diff with
        | `Unequal _ -> Map.remove defs v
        | _ -> defs) in
  let rec run ss defs = function
    | [] -> List.rev ss, defs
    | Move (v,e) :: bil ->
      let defs, e = match substitute defs e with
        | Int _ as e -> Map.add defs v e, e
        | e -> Map.remove defs v, e in
      run (Move (v,e) :: ss) defs bil
    | If (cond,yes,no) :: bil ->
      let yes,defs_yes = run [] defs yes in
      let no, defs_no  = run [] defs no  in
      let cond = substitute defs cond in
      let defs' = match cond with
        | Int w when Word.is_one  w -> defs_yes
        | Int w when Word.is_zero w -> defs_no
        | _ ->
          let defs = remove_unequal defs defs_yes in
          remove_unequal defs defs_no in
      run (If (cond,yes,no) :: ss) defs' bil
    | While (cond,body) :: bil ->
      let defs = remove_forward_defs body defs in
      let body, defs' = run [] defs body in
      let cond = substitute defs cond in
      let defs' = match cond with
        | Int w when Word.is_one w -> defs'
        | _ -> remove_unequal defs defs' in
      run (While (cond,body) :: ss) defs' bil
    | Jmp dst :: bil ->
      let dst = substitute defs dst in
      run (Jmp dst :: ss) defs bil
    | s :: bil -> run (s :: ss) defs bil in
  inline @@ fst @@ run [] Var.Map.empty bil
