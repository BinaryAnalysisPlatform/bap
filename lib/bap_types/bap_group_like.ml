open Core_kernel
open Bap_common
open Bap_bil
open Bap_visitor

open Binop
open Unop
open Exp

module Infer = Bap_type_infer
module Type_error = Bap_type_error
module Word = Bitvector
module Var = Bap_var

type t =
  | Sum of exp list

type group = {
  exprs : exp * exp list;
  likes : exp -> bool;
}

let is_associative = function
  | PLUS | TIMES | AND | OR | XOR -> true
  | _ -> false

let is_associative op op' =
  is_associative op && compare_binop op op' = 0

let is_distributive op op' = match op,op' with
  | TIMES, (PLUS | MINUS) -> true
  | _ -> false

module Group = struct

  type t = group

  let exp_base e =
    let width = match Infer.infer_exn e with
      | Type.Imm w -> w
      | _ -> Type_error.expect_imm () in
    let m1 = Word.of_int ~width (-1) in
    let one = Word.one width in
    let rec run base_k = function
      | UnOp(NEG, e) -> run Word.(base_k * m1) e
      | BinOp(TIMES, Int k, e)
      | BinOp(TIMES, e, Int k) -> run Word.(base_k * k) e
      | e -> Word.signed base_k, e in
    run one e

  let is_like e e' =
    let base e = snd (exp_base e) in
    match e, e' with
    | e, e' when Bap_exp.equal (base e) (base e') -> true
    | BinOp (op, x, y), BinOp (op', w, z) when is_associative op op' ->
      (Bap_exp.equal x w && Bap_exp.equal y z) || (Bap_exp.equal x z && Bap_exp.equal y w)
    | e,e' -> Bap_exp.equal e e'

  let create e = { exprs = e,[]; likes = is_like e; }

  let add_to_group t e' =
    if t.likes e' then
      let e,exs = t.exprs in
      Some {t with exprs = e, e' :: exs}
    else None

  let sum_coeffs t =
    let e, exps = t.exprs in
    let c, e = exp_base e in
    let cs = List.map exps ~f:(fun e -> fst @@ exp_base e) in
    let c = Word.signed @@ List.fold ~f:Word.(+) cs ~init:c in
    if Word.is_positive c then c, e
    else Word.abs c, UnOp(NEG, e)

  let empty_pull = []

  let add_to_pull pull e =
    let rec add acc = function
      | [] -> List.rev @@ create e :: acc
      | group :: pull ->
        match add_to_group group e with
        | None -> add (group :: acc) pull
        | Some group' -> (List.rev (group' :: acc)) @ pull in
    add [] pull

  let process exps =
    let rec group_by_coeff acc = function
      | [] -> List.rev acc
      | (c,e) :: ungroupped ->
        let groupped, ungroupped =
          List.partition_map ungroupped
            ~f:(fun ((c', e') as x) ->
                if Word.equal c c' then `Fst e'
                else `Snd x) in
        let acc = (c, e :: groupped) :: acc in
        group_by_coeff acc ungroupped in
    List.fold exps ~init:empty_pull ~f:add_to_pull |>
    List.map ~f:sum_coeffs |>
    group_by_coeff []

end

let positive e =
  (object(self)
    inherit exp_mapper
    method! map_binop op x y = match op with
      | MINUS -> BinOp(PLUS, self#map_exp x, UnOp(NEG, self#map_exp y))
      | _ -> BinOp(op, self#map_exp x, self#map_exp y)
  end)#map_exp e

let sum x y =
  let rec run x =
    match x with
    | BinOp(PLUS, a, b) -> run a @ run b
    | BinOp (op', Int q, BinOp(op'', x, y))
    | BinOp (op', BinOp(op'', x, y), Int q)
      when is_distributive op' op'' && is_associative PLUS op'' ->
      run (BinOp(op', Int q, x)) @ run (BinOp(op', Int q, y))
    | x -> [x] in
  Sum (run x @ run y)

let fold_sum = function
  | [] -> assert false
  | [e] -> e
  | fst :: snd :: exps ->
    let init = BinOp(PLUS, fst, snd) in
    List.fold exps ~init ~f:(fun e x -> BinOp (PLUS, e, x))

let to_exp = function
  | Sum es -> fold_sum es

let group = function
  | Sum exs ->
    let groups =
      Group.process exs |>
      List.fold ~init:[]
        ~f:(fun acc (k, exps) ->
            let e = match k, exps with
              | k, exps when Word.is_one k -> fold_sum exps
              | _ -> BinOp (TIMES, Int k, fold_sum exps) in
            e :: acc) in
    Sum (List.rev groups)

let sum_like x y = to_exp @@ group @@ sum x y

let run e =
  let apply e = (object(self)
    inherit exp_mapper
    method! map_binop op x y = match op with
      | PLUS -> sum_like (self#map_exp x) (self#map_exp y)
      | _ -> BinOp(op, self#map_exp x, self#map_exp y)
  end)#map_exp e in
  positive e |> apply
