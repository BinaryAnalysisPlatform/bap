open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor

module Word = Bitvector

let find (finder : 'a #finder) ss : 'a option =
  finder#find_in_bil ss
let exists finder ss = finder#find_in_bil ss = Some ()
let iter (visitor : unit #visitor) ss = visitor#run ss ()
let fold (visitor : 'a #visitor) ~init ss = visitor#run ss init
let map m = m#run

let is_assigned ?(strict=false) x = exists (object(self)
    inherit [unit] finder
    method! enter_move y _ cc =
      if Bap_var.(x = y) && not(strict && under_condition)
      then cc.return (Some ());
      cc
  end)

class reference_finder x = object(self)
  inherit [unit] finder
  method! visit_move y rhs goto =
    let goto = self#visit_exp rhs goto in
    if Bap_var.(x = y)
    then goto.return None
    else goto
  method! enter_var y cc =
    if Bap_var.(x = y) then cc.return (Some ()); cc
end

let is_referenced x = exists (new reference_finder x)

let prune_unreferenced
    ?(such_that=(fun _ -> false))
    ?(physicals=false)
    ?(virtuals=false) stmt =
  let prune x =
    Bap_var.(physicals && is_physical x ||
             virtuals && is_virtual  x ||
             such_that x) in
  let rec loop ss = function
    | [] -> List.rev ss
    | Stmt.Move (x,_) as s :: xs when prune x ->
      if is_referenced x xs then loop (s::ss) xs else loop ss xs
    | s :: xs -> loop (s::ss) xs in
  loop [] stmt

class negative_normalizer = object
  inherit mapper as super
  method! map_binop op e1 e2 = match op,e2 with
    | Binop.PLUS, Exp.Int arg
      when Word.(is_negative (signed arg)) ->
      let (-) = Bap_exp.Infix.(-) in
      (e1 - Exp.Int Word.(abs (signed arg)))
    | _ -> super#map_binop op e1 e2
end

let normalize_negatives = (new negative_normalizer)#run

module Addr = Bitvector
let rec addr_intersection (x,xs) (y,ys) =
  let open Addr in
  let xe = x ++ xs and ye = y ++ ys in
  let ze = min xe ye in
  if x <= y then
    if ze > y then Some (y, ok_exn (to_int (ze - y)))
    else None
  else addr_intersection (y,ys) (x,xs)

class substitution x y = object(self)
  inherit mapper as super
  method! map_let z ~exp ~body =
    if Bap_var.(z = x)
    then super#map_let z ~exp:(self#map_exp exp) ~body
    else super#map_let z ~exp:(self#map_exp exp) ~body:
        (super#map_exp body)

  method! map_var z =
    match super#map_var z with
    | Exp.Var z when Bap_var.(z = x) -> y
    | z -> z
end

let substitute_var x y ss =
  let r = new substitution x y in
  let rec loop acc = function
    | Stmt.Move (z,e) :: ss when Bap_var.(z = x) ->
      List.rev_append acc (Stmt.Move (z,r#map_exp e)::ss)
    | Stmt.If (c,t,e) :: ss ->
      loop (Stmt.If (r#map_exp c, loop [] t, loop [] e)::acc) ss
    | Stmt.While (c,b) :: ss ->
      loop (Stmt.While (r#map_exp c, loop [] b)::acc) ss
    | s :: ss -> loop (r#map_stmt s @ acc) ss
    | [] -> List.rev acc in
  loop [] ss

include struct
  open Exp
  let expi = new Bap_expi.t
  let ctxt = new Bap_expi.context

  class constant_folder = object
    inherit mapper as super
    method! map_exp e =
      let r = Bap_monad.State.eval (expi#eval_exp e) ctxt in
      match Bap_result.value r  with
      | Bap_result.Imm w -> Exp.Int w
      | _ -> super#map_exp e

    method! map_binop op e1 e2 =
      let open Binop in
      let zero v1 v2 = match v1,v2 with
        | Int x,_ |_,Int x  -> Int (Word.zero (Word.bitwidth x))
        | Var v,_ | _, Var v ->
          begin match Bap_var.typ v with
            | Type.Imm width -> Int (Word.zero width)
            | Type.Mem _ -> super#map_binop op e1 e2
          end
        | _ -> super#map_binop op e1 e2 in
      let equal x y = compare_exp x y = 0 in
      let open Bap_exp.Exp in
      match op, e1, e2 with
      | (AND|OR), e1, e2 when equal e1 e2 -> e1
      | XOR, e1, e2 when equal e1 e2 -> zero e1 e2
      | EQ, e1, e2 when equal e1 e2 -> Int Word.b1
      | NEQ, e1, e2 when equal e1 e2 -> Int Word.b0
      | (LT|SLT), e1, e2 when equal e1 e2 -> Int Word.b0
      | (PLUS|LSHIFT|RSHIFT|ARSHIFT|OR|XOR), Int v, e
      | (PLUS|MINUS|LSHIFT|RSHIFT|ARSHIFT|OR|XOR), e, Int v
        when Word.is_zero v -> e
      | (TIMES|AND),e,Int v
      | (TIMES|AND), Int v, e when Word.is_one v -> e
      | (TIMES|AND), e, Int v
      | (TIMES|AND), Int v, e when Word.is_zero v -> Int v
      | (OR|AND), v1, v2 when equal v1 v2 -> v1
      | (XOR), v1, v2 when equal v1 v2 -> zero v1 v2
      | EQ, e, Int v when Word.(v = b1) -> e
      | NEQ,e, Int v when Word.(v = b0) -> e
      | EQ, e, Int v when Word.(v = b0) -> super#map_unop Unop.NOT e
      | NEQ,e, Int v when Word.(v = b1) -> super#map_unop Unop.NOT e
      | op, Int v, e when Bap_exp.Binop.is_commutative op ->
        super#map_binop op e (Int v)
      | PLUS, BinOp (PLUS, a, Int b), Int c ->
        BinOp (PLUS, a, super#map_binop PLUS (Int b) (Int c))
      | PLUS, BinOp (MINUS, a, Int b), Int c ->
        BinOp (MINUS, a, super#map_binop MINUS (Int b) (Int c))
      | MINUS, BinOp (MINUS, a, Int b), Int c ->
        BinOp (MINUS, a, super#map_binop PLUS (Int b) (Int c))
      | _ -> super#map_binop op e1 e2

    method! map_unop op arg = match arg with
      | UnOp (op',arg) when op = op' -> arg
      | _ -> super#map_unop op arg

    method! map_let var ~exp ~body =
      match exp with
      | Int v ->  (new substitution var exp)#map_exp body
      | _ -> super#map_let var ~exp ~body

    method! map_ite ~cond ~yes ~no =
      match cond with
      | Int v -> if Word.is_zero v then no else yes
      | _ -> super#map_ite ~cond ~yes ~no

    method! map_if ~cond ~yes ~no = match cond with
      | Int v -> if Word.is_zero v then no else yes
      | _ -> super#map_if ~cond ~yes ~no

    method! map_while ~cond bil = match cond with
      | Int v -> if Word.is_zero v then [] else bil
      | _ -> super#map_while ~cond bil
  end
end
let fold_consts = (new constant_folder)#run

let fix compare f x  =
  let rec loop slow fast =
    if compare slow fast = 0 then fast
    else
      let fast' = f fast in
      if compare fast' fast = 0 then fast
      else loop (f slow) (f fast) in
  loop x (f x)

let fixpoint = fix compare_bil

class rewriter x y = object
  inherit mapper as super
  method! map_exp z =
    let z = super#map_exp z in
    if Bap_exp.(z = x) then y else z
end

let substitute x y = (new rewriter x y)#run

module Trie = struct
  type normalized_bil = bil

  let vars = String.Table.create ()

  let pruned ty = Exp.Unknown ("<pruned>", ty)

  let normalize_values =
    map (object inherit mapper
      method! map_sym var =
        let name = Bap_var.name var in
        let ty = Bap_var.typ var in
        String.Table.find_or_add vars name
          ~default:(fun () -> Bap_var.create name ty)
      method! map_int w =
        let ty = Type.Imm (Word.bitwidth w) in
        pruned ty
    end)

  let prune x = prune_unreferenced
      ~virtuals:true x

  let simplify = List.map ~f:fixpoint [
      prune;
      normalize_negatives;
      fold_consts;
    ] |> List.reduce_exn ~f:Fn.compose |> fixpoint

  let normalize ?(subst=[]) bil =
    List.fold subst ~init:bil ~f:(fun bil (x,y) -> substitute x y bil)
    |> simplify |> normalize_values

  module Normalized = Trie.Make(struct
      type t = bil
      type token = stmt with bin_io, sexp, compare
      let length  = List.length
      let nth_token = List.nth_exn
      let token_hash = Hashtbl.hash
    end)

  include Normalized
end

module VS = Bap_var.Set

module Exp = struct
  let find (finder : 'a #finder) es : 'a option =
    finder#find_in_exp es
  let exists finder ss = finder#find_in_exp ss = Some ()
  let iter (visitor : unit #visitor) ss = visitor#visit_exp ss ()
  let fold (visitor : 'a #visitor) ~init ss = visitor#visit_exp ss init
  let map m = m#map_exp
  let is_referenced x = exists (new reference_finder x)
  let normalize_negatives = (new negative_normalizer)#map_exp
  let fold_consts = (new constant_folder)#map_exp
  let fixpoint = fix compare_exp

  let substitute x y = map (new rewriter x y)

  let free_vars exp = fst @@ fold ~init:(VS.empty,[]) (object
      inherit [VS.t * var list] visitor

      method! enter_var var (vars,stack) =
        if List.exists stack ~f:(fun x -> Bap_var.(x = var))
        then (vars,stack) else Set.add vars var,stack

      method! enter_let var ~exp:_ ~body:_ (vars,stack) =
        (vars, var::stack)

      method! leave_let var ~exp:_ ~body:_ (vars,stack) =
        (vars, List.tl_exn stack)
    end) exp

  include struct
    open Bap_expi
    let eval exp =
      let expi = new t and ctxt = new context in
      Bap_monad.State.eval (expi#eval_exp exp) ctxt |> Bap_result.value
  end
end

module Stmt = struct
  let find (finder : 'a #finder) s : 'a option =
    finder#find_in_bil [s]
  let exists finder ss = finder#find_in_bil [ss] = Some ()
  let iter (visitor : unit #visitor) ss = visitor#visit_stmt ss ()
  let fold (visitor : 'a #visitor) ~init ss = visitor#visit_stmt ss init
  let map (m : #mapper) = m#map_stmt
  let assigns ?strict x stmt = is_assigned ?strict x [stmt]
  let is_referenced x ss = is_referenced x [ss]
  let fixpoint = fix compare_stmt
  let substitute x y = map (new rewriter x y)

  let rec free_vars (s : stmt) = match s with
    | Stmt.Move (_,e)
    | Stmt.Jmp e -> Exp.free_vars e
    | Stmt.While (e,ss) ->
      VS.union_list @@ Exp.free_vars e :: List.map ss ~f:free_vars
    | Stmt.If (e,s1,s2) ->
      VS.union_list @@ Exp.free_vars e :: List.map (s1@s2) ~f:free_vars
    | Stmt.Special _
    | Stmt.CpuExn _ -> VS.empty

  let bil_free_vars bil =
    let update news vars kill =
      VS.union vars (VS.diff news kill) in
    fst @@ List.fold bil ~init:(VS.empty,VS.empty)
      ~f:(fun (vars,kill) -> function
          | Stmt.Move (v,e) ->
            update (Exp.free_vars e) vars kill, VS.add kill v
          | stmt -> update (free_vars stmt) vars kill, kill)

  let eval stmts ctxt =
    let bili = new Bap_bili.t in
    Bap_monad.State.exec (bili#eval stmts) ctxt
end

let free_vars = Stmt.bil_free_vars
