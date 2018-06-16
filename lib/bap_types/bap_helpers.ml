open Core_kernel.Std
open Monads.Std
open Bap_common
open Bap_bil
open Bap_visitor

module Eff = Bap_eff
module Normalize = Bap_normalize
module Simpl = Bap_simpl
module Size = Bap_size
module Type_error = Bap_type_error
module Var = Bap_var
module Word = Bitvector

let find finder ss : 'a option = finder#find ss
let find_stmt f s = find f [s]
let exists finder ss = Option.is_some (finder#find ss)
let iter (visitor : unit #bil_visitor) ss = visitor#run ss ()
let fold (visitor : 'a #bil_visitor) ~init ss = visitor#run ss init
let map m = m#run
let map_exp m = m#map_exp
let map_stmt m = m#map_stmt
let is0 = Word.is_zero and is1 = Word.is_one
let ism1 x = Word.is_zero (Word.lnot x)

let is_assigned ?(strict=false) x = exists (object
    inherit [unit] bil_finder
    method! enter_move y _ cc =
      if Bap_var.(x = y) && not(strict && under_condition)
      then cc.return (Some ());
      cc
  end)

class exp_reference_finder x = object
  inherit [unit] exp_finder
  method! enter_var y cc =
    if Bap_var.(x = y) then cc.return (Some ()); cc
end

class reference_finder x = object(self)
  inherit [unit] bil_finder
  val exp_reference = new exp_reference_finder x
  method! visit_move y rhs goto =
    let goto = self#visit_exp rhs goto in
    if Bap_var.(x = y)
    then goto.return None
    else goto

  method! enter_var y cc =
    exp_reference#enter_var y cc
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
  inherit bil_mapper as super
  method! map_binop op e1 e2 = match op,e2 with
    | Binop.PLUS, Exp.Int arg
      when Word.(is_negative (signed arg)) ->
      let (-) = Bap_exp.Infix.(-) in
      (e1 - Exp.Int Word.(abs (signed arg)))
    | _ -> super#map_binop op e1 e2
end

let normalize_negatives = (new negative_normalizer)#run

class substitution x y = object(self)
  inherit bil_mapper as super
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
  inherit bil_mapper as super
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
    map (object inherit bil_mapper
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

  let simplify = [
    prune;
    normalize_negatives;
    Simpl.bil;
  ] |> List.reduce_exn ~f:Fn.compose

  let normalize ?(subst=[]) bil =
    List.fold subst ~init:bil ~f:(fun bil (x,y) -> substitute x y bil)
    |> simplify |> normalize_values

  module Normalized = Trie.Make(struct
      type t = bil
      type token = stmt [@@deriving bin_io, sexp, compare]
      let length  = List.length
      let nth_token = List.nth_exn
      let token_hash = Hashtbl.hash
    end)

  include Normalized
end

module VS = Bap_var.Set

module Constant_folder = struct
  open Exp
  class main = object
    inherit bil_mapper as super
    method! map_exp e = super#map_exp e |> Simpl.exp
    method! map_unop op x = super#map_unop op x |> Simpl.exp
    method! map_let var ~exp ~body =
      super#map_let var ~exp ~body |> Simpl.exp
    method! map_ite ~cond ~yes ~no =
      super#map_ite ~cond ~yes ~no |> Simpl.exp
    method! map_if ~cond ~yes ~no =
      super#map_if ~cond ~yes ~no |> Simpl.bil
    method! map_while ~cond bil =
      super#map_while ~cond bil |> Simpl.bil
  end
end

module Exp = struct
  open Exp
  class state = exp_state
  class ['a] visitor = ['a] exp_visitor
  class ['a] finder  = ['a] exp_finder
  class mapper = exp_mapper

  let find (finder : 'a #finder) es : 'a option =
    finder#find es
  let exists finder ss = finder#find ss = Some ()
  let iter (visitor : unit #visitor) ss = visitor#visit_exp ss ()
  let fold (visitor : 'a #visitor) ~init ss = visitor#visit_exp ss init
  let map m = m#map_exp
  let is_referenced x = exists (new exp_reference_finder x)
  let normalize_negatives = (new negative_normalizer)#map_exp
  let fold_consts = Simpl.exp ~ignore:[Eff.read]
  let fixpoint = fix compare_exp
  let normalize = Normalize.normalize_exp
  let substitute x y = map (new rewriter x y)

  let free_vars exp = fst @@ fold ~init:(VS.empty,[]) (object
      inherit [VS.t * var list] visitor

      method! enter_var var (vars,stack) =
        if List.exists stack ~f:(fun x -> Bap_var.(x = var))
        then (vars,stack) else Set.add vars var,stack

      method! enter_let var ~exp:_ ~body:_ (vars,stack) =
        (vars, var::stack)

      method! leave_let _var ~exp:_ ~body:_ (vars,stack) =
        (vars, List.tl_exn stack)
    end) exp
end

module Stmt = struct
  class state = stmt_state
  class ['a] visitor = ['a] bil_visitor
  class ['a] finder  = ['a] bil_finder
  class mapper = bil_mapper

  let find (finder : 'a #finder) s : 'a option =
    finder#find [s]
  let exists finder ss = finder#find [ss] = Some ()
  let iter (visitor : unit #visitor) ss = visitor#visit_stmt ss ()
  let fold (visitor : 'a #visitor) ~init ss = visitor#visit_stmt ss init
  let map (m : #mapper) = m#run
  let assigns ?strict x stmt = is_assigned ?strict x [stmt]
  let is_referenced x ss = is_referenced x [ss]
  let fixpoint = fix compare_stmt
  let substitute x y = (new rewriter x y)#map_stmt

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

  class constant_folder = Constant_folder.main
  let normalize  = Normalize.bil
end

let free_vars = Stmt.bil_free_vars
let fold_consts = Simpl.bil ~ignore:[Eff.read]
let normalize = Normalize.bil
