open Core_kernel.Std
open Monads.Std
open Bap_common
open Bap_bil
open Bap_visitor

module Word = Bitvector
module Var = Bap_var
module Size = Bap_size
module Type_error = Bap_type_error
module Type_infer = Bap_type_infer
module Group_like = Bap_group_like

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


(* maps BIL expressions to Word operations *)
module Apply = struct
  open Bap_bil
  open Binop
  open Unop
  let is_shift = function
    | LSHIFT | RSHIFT | ARSHIFT -> true
    | _ -> false

  let unop op u = match op with
    | NEG -> Word.neg u
    | NOT -> Word.lnot u

  let binop op u v =
    let open Word in
    if Int.(bitwidth u <> bitwidth v) && not (is_shift op)
    then failwithf "binop type error - %s %s %s"
        (to_string u)
        (Bap_exp.Binop.string_of_binop op)
        (to_string v) ();
    match op with
    | PLUS -> u + v
    | MINUS -> u - v
    | TIMES -> u * v
    | DIVIDE -> u / v
    | SDIVIDE -> signed u / signed v
    | MOD -> u mod v
    | SMOD -> signed u mod signed v
    | LSHIFT -> u lsl v
    | RSHIFT -> u lsr v
    | ARSHIFT -> u asr v
    | AND -> u land v
    | OR -> u lor v
    | XOR -> u lxor v
    | EQ -> Bitvector.(of_bool (u = v))
    | NEQ -> Bitvector.(of_bool (u <> v))
    | LT -> Bitvector.(of_bool (u < v))
    | LE -> Bitvector.(of_bool (u <= v))
    | SLT -> Bitvector.(of_bool (signed u < signed v))
    | SLE  -> Bitvector.(of_bool (signed u <= signed v))

  let cast ct sz u =
    let ext = Bitvector.extract_exn in
    match ct with
    | Cast.UNSIGNED -> ext ~hi:Int.(sz - 1) u
    | Cast.SIGNED   -> ext ~hi:Int.(sz - 1) (Bitvector.signed u)
    | Cast.HIGH     -> ext ~lo:Int.(Bitvector.bitwidth u - sz) u
    | Cast.LOW      -> ext ~hi:Int.(sz - 1) u

end

(*
  The effect analysis kind of contradicts the official operational
  semantics of BIL, as it is stated there that expressions do not
  manifest any effects.

  This module is one of the first steps in the direction of new BIL,
  that will be easier to analyze, and that will have semantics that
  corresponds to real hardware not to some abstract non-implementable
  machine. *)
module Eff = struct
  open Bap_bil
  open Binop
  open Unop
  open Exp
  open Stmt
  open Cast

  type eff = Reads | Loads | Stores | Raises
  type t = eff Set.Poly.t

  let none : t = Set.Poly.empty
  let read  = Set.Poly.singleton Reads
  let load  = Set.Poly.singleton Loads
  let store = Set.Poly.singleton Stores
  let raise = Set.Poly.singleton Raises
  let join_eff = Set.union

  let has_effects eff = Set.mem eff Raises || Set.mem eff Stores
  let has_coeffects eff = Set.mem eff Reads || Set.mem eff Loads
  let idempotent = Set.is_empty

  let width x = match Type_infer.infer_exn x with
    | Type.Imm x -> x
    | Type.Mem _ -> failwith "width is not for memory"

  (* approximates a number of non-zero bits in a bitvector.  *)
  module Nz = struct
    open Cast

    (* an approximation of a set of non-zero bits  *)
    type nzbits =
      | Empty                   (* the set is empty *)
      | Maybe                   (* the set might be non-empty *)
      | Exists                  (* there exists at least one non-zero bit *)
      | Forall                  (* all bits are set (the [~0] value)  *)

    (* assumes that zero and unit elements are already applied, thus
       we shouldn't see expressions where all operands are ints. This
       actually makes a function kind of internal, as we can't publish
       a function with such a complex precondition. *)
    let rec bits = function
      | Var _ -> Maybe
      | Int x when is0 x -> Empty
      | Int x when ism1 x -> Forall
      | Int _ -> Exists
      | Load _ -> Maybe
      | Store _ -> Maybe         (* shouldn't occur at all *)
      | Cast (t,x,y) -> cast t x y
      | BinOp (op,x,y) -> binop op x y
      | UnOp (NEG,x) -> bitneg x
      | UnOp (NOT,x) -> bitnot x
      | Let _ -> Maybe
      | Extract (hi,lo,x) -> extract hi lo x
      | Unknown _ -> Maybe
      | Ite (c,x,y) -> ite c x y
      | Concat (x,y) -> concat x y
    and cast t _ x = match t with
      | SIGNED|UNSIGNED -> bits x
      | HIGH|LOW -> narrow x
    and extract hi lo x =
      if hi - lo + 1 >= width x then bits x else narrow x
    and binop op x y = match op with
      | OR -> join x y
      | _ -> Maybe
    and concat x y = match bits x, bits y with
      | Exists,_|_,Exists -> Exists
      | _ -> Maybe
    and narrow x = match bits x with
      | Exists -> Maybe
      | x -> x
    and bitneg x = match bits x with
      | Forall -> Exists
      | b -> b
    and bitnot x = match bits x with
      | Forall -> Empty
      | Empty -> Forall
      | x -> x
    and join x y = match bits x, bits y with
      | Forall,_ | _,Forall -> Forall
      | Exists,_ | _,Exists -> Exists
      | Maybe,_ | _,Maybe -> Maybe
      | Empty,Empty -> Empty
    and ite _ x y = match bits x, bits y with
      | Exists,Forall| Forall,Exists -> Exists
      | x,y when x = y -> x
      | _ -> Maybe
  end

  (* although the expression language are meant to be effectless,
     there still can be some sort of effects, e.g., memory load and
     store operations and division by zero. And although, the main
     effect of the store/load operations is encoded in the memory
     value, there can be other side-effects, such as page fault for
     example. In fact, even reading from a memory address can be
     considered an effect, as it may trigger a complex machinery,
     especially if the memory is mapped.

     Thus we need to compute an approximation of an effect, so that
     expressions that have any effects are preserved and are not
     reordered or duplicated.

     Note, the function inherits preconditions from the [Nz.bits].
  *)

  let rec eff = function
    | Var _ -> read
    | Int _ -> none
    | Unknown _ -> none
    | Load (m,a,_,_) -> all [raise; load; eff m; eff a]
    | Store (m,a,x,_,_) -> all [raise; store; eff m; eff a; eff x]
    | BinOp ((DIVIDE|SDIVIDE|MOD|SMOD),x,y) -> all [div y; eff x; eff y]
    | BinOp (_,x,y)
    | Concat (x,y) -> all [eff x; eff y]
    | Ite (x,y,z) -> all [eff x; eff y; eff z]
    | UnOp (_,x)
    | Cast (_,_,x)
    | Extract (_,_,x) -> eff x
    | Let (_,_,_) -> assert false (* must be let-normalized *)
  and div y = match Nz.bits y with
    | Nz.Maybe | Nz.Empty -> raise
    | _ -> none
  and all = List.fold_left ~init:none ~f:join_eff

  let compute = eff
  let reads t = Set.mem t Reads
  let loads t = Set.mem t Loads
  let stores t = Set.mem t Stores
  let raises t = Set.mem t Raises
  let of_list : t list -> t = Set.Poly.union_list
end

module Simpl = struct
  open Bap_bil
  open Binop
  open Unop
  open Exp
  open Stmt
  open Cast


  let zero width = Int (Word.zero width)
  let ones width = Int (Word.ones width)
  let nothing _ = false


  (* requires: let-free, simplifications(
        constant-folding,
        neutral-element-elimination,
        zero-element-propagation)  *)
  let removable ignore x =
    Set.is_subset (Eff.compute x) ~of_:(Eff.of_list ignore)

  let is_associative = function
    | PLUS | TIMES | AND | OR | XOR -> true
    | _ -> false

  let is_associative op op' =
    is_associative op && compare_binop op op' = 0

  let is_distributive op op' = match op,op' with
    | TIMES, (PLUS | MINUS) -> true
    | _ -> false

  let pretify e =
    let is_int = function
      | Int _ -> true
      | _ -> false in
    (object(self)
      inherit exp_mapper as super
      method! map_binop op x y =
        match op, self#map_exp x, self#map_exp y with
        | PLUS, Int q, x when Word.(is_negative (signed q)) ->
          self#map_exp (BinOp (PLUS, x, Int q))
        | PLUS, x, UnOp(NEG, y) -> self#map_exp Bap_exp.Infix.(x - y)
        | PLUS, UnOp(NEG, x), y -> self#map_exp Bap_exp.Infix.(y - x)
        | TIMES, x, UnOp(NEG, y) ->
          self#map_exp (UnOp (NEG,(BinOp (TIMES, x, y))))
        | TIMES, x, Int q when not (is_int x) ->
          self#map_exp @@ BinOp(TIMES, Int q, x)
        | op,x,y -> super#map_binop op x y
    end)#map_exp e |> (new negative_normalizer)#map_exp

  let exp ?(ignore=[]) e =
    let removable = removable ignore in
    let infer_width x = match Type_infer.infer_exn x with
      | Type.Imm w -> w
      | Type.Mem _ -> failwith "unexpected Mem type" in
    let rec exp = function
      | Load (m,a,e,s) -> Load (exp m, exp a, e, s)
      | Store (m,a,v,e,s) -> Store (exp m, exp a, exp v, e, s)
      | BinOp (op,x,y) -> binop op x y
      | UnOp (op,x) -> unop op x
      | Var _ | Int _  | Unknown (_,_) as const -> const
      | Cast (t,s,x) -> cast t s x
      | Let (v,x,y) -> Let (v, exp x, exp y)
      | Ite (x,y,z) -> Ite (exp x, exp y, exp z)
      | Extract (h,l,x) -> extract h l x
      | Concat (x,y) -> concat x y
    and concat x y = match exp x, exp y with
      | Int x, Int y -> Int (Word.concat x y)
      | x,y -> Concat (x,y)
    and cast t s x = match exp x with
      | Int w -> Int (Apply.cast t s w)
      | Var _ as v ->
        if infer_width v = s then v
        else Cast (t,s,v)
      | x -> Cast (t,s,x)
    and extract hi lo x = match exp x with
      | Int w -> Int (Bitvector.extract_exn ~hi ~lo w)
      | Var _ as v ->
        if infer_width v = hi + 1 && lo = 0 then v
        else Extract (hi,lo,v)
      | x -> Extract (hi,lo,x)
    and unop op x =
      let apply op x = Int (Apply.unop op x) in
      let lnot = Bap_exp.Infix.lnot in
      match op, exp x with
      | op, Int x -> apply op x
      | op, UnOp (op', x) when compare_unop op op' = 0 -> x
      | NOT, BinOp (AND, Int q, x) -> BinOp (OR, apply NOT q, lnot x)
      | NOT, BinOp (AND, x, Int q) -> BinOp (OR, lnot x, apply NOT q)
      | NOT, BinOp (OR, Int q, x) -> BinOp (AND, apply NOT q, lnot x)
      | NOT, BinOp (OR, x, Int q) -> BinOp (AND, lnot x, apply NOT q)
      | NOT, BinOp (LT, x, y) -> BinOp(LE, y, x)
      | NOT, BinOp (LE, x, y) -> BinOp(LT, y, x)
      | NOT, BinOp (EQ, x, y) -> BinOp(NEQ,x, y)
      | NOT, BinOp (NEQ,x, y) -> BinOp(EQ, x, y)
      | NEG, BinOp (PLUS, x, Int q)
      | NEG, BinOp (PLUS, Int q, x) -> exp (BinOp (MINUS, apply op q, x))
      | op, x -> UnOp(op, x)
    and binop op x y =
      let width = infer_width x in
      let keep op x y = BinOp(op,x,y) in
      let int f = function Int x -> f x | _ -> false in
      let is0 = int is0 and is1 = int is1 and ism1 = int ism1 in
      let (=) x y = compare_exp x y = 0 && removable x in
      let (+) = Bap_exp.Infix.(+) in
      let apply op x y = Int (Apply.binop op x y) in
      let neg x = UnOp (Bap_exp.Unop.neg, x) in
      match op, exp x, exp y with
      | op, Int x, Int y -> apply op x y
      | PLUS,x,y  when is0 x -> y
      | PLUS,x,y  when is0 y -> x
      | PLUS, x, UnOp(NEG, y) when x = y -> zero width
      | PLUS, UnOp(NEG, x), y when x = y -> zero width
      | PLUS, x, UnOp(NOT, y) when x = y -> ones width
      | PLUS, UnOp(NOT, x), y when x = y -> ones width
      | PLUS, UnOp(NEG,x), UnOp(NEG, y) -> neg (x + y)
      | MINUS,x,y when is0 x -> neg y
      | MINUS,x,y when is0 y -> x
      | MINUS,x,y when x = y -> zero width
      | MINUS,x,y -> exp (x + neg y)
      | TIMES,x,y when is0 x && removable y -> x
      | TIMES,x,y when is0 y && removable x -> y
      | TIMES,x,y when is1 x -> y
      | TIMES,x,y when is1 y -> x
      | (DIVIDE|SDIVIDE),x,y when is1 y -> x
      | (MOD|SMOD),_,y when is1 y -> zero width
      | (LSHIFT|RSHIFT|ARSHIFT),x,y when is0 y -> x
      | (LSHIFT|RSHIFT|ARSHIFT),x,_ when is0 x -> x
      | ARSHIFT,x,_ when ism1 x -> x
      | AND,x,y when is0 x && removable y -> x
      | AND,x,y when is0 y && removable x -> y
      | AND,x,y when ism1 x -> y
      | AND,x,y when ism1 y -> x
      | AND,x,y when x = y -> x
      | OR, x,y when is0 x -> y
      | OR, x,y when is0 y -> x
      | OR, x,y when ism1 x && removable y -> x
      | OR, x,y when ism1 y && removable x -> y
      | OR, x,y when x = y -> x
      | XOR,x,y when x = y -> zero width
      | XOR,x,y when is0 x -> y
      | XOR,x,y when is0 y -> x
      | EQ, x,y when x = y -> Int Word.b1
      | NEQ,x,y when x = y -> Int Word.b0
      | (LT|SLT), x, y when x = y -> Int Word.b0
      | (LE|SLE), x, y when x = y -> Int Word.b1
      | op, BinOp(op', x, Int p), Int q when is_associative op op' ->
        exp @@ BinOp (op, x, apply op p q)
      | op, BinOp(op', x, Int q), BinOp(op'', y, Int p)
        when is_associative op op' && is_associative op op'' ->
        exp @@ BinOp (op, BinOp (op, x, y), (apply op q p))
      | op, BinOp(op', x, Int p), y
      | op, BinOp(op', Int p, x), y when is_associative op op' ->
        exp @@ BinOp (op, BinOp (op, x, y), Int p)
      | op, y, BinOp(op', x, Int p)
      | op, y, BinOp(op', Int p, x) when is_associative op op' ->
        exp @@ BinOp (op, BinOp (op, y, x), Int p)
      | op, BinOp(op', x, Int p), Int q
      | op, Int q, BinOp(op', x, Int p) when is_distributive op op' ->
        exp @@ BinOp (op',BinOp(op, Int q, x), apply op p q)
      | op, BinOp(op', Int p, x), Int q
      | op, Int q, BinOp(op', Int p, x) when is_distributive op op' ->
        exp @@ BinOp (op',apply op p q, BinOp(op, Int q, x))
      | op,x,y -> keep op x y in
    exp e |> pretify

  let exp ?ignore e =
    exp ?ignore e |> Group_like.run |> exp ?ignore

  let bil ?ignore =
    let exp x = exp ?ignore x in
    let rec stmt = function
      | Move (v,x) -> [Move (v, exp x)]
      | Jmp x -> [Jmp (exp x)]
      | While (c,ss) -> while_ c ss
      | If (c,ts,fs) -> if_ c ts fs
      | s -> [s]
    and if_ c ts fs = match exp c with
      | Int x ->
        if Word.is_zero x then bil fs else bil ts
      | c -> [If (exp c, bil ts, bil fs)]
    and while_  c ss = match exp c with
      | Int x when Word.is_zero x -> []
      | c -> [While (c, bil ss)]
    and bil = List.concat_map ~f:stmt in
    bil

  let stmt ?ignore x = bil ?ignore [x]
end

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

module Normalize = struct
  open Bap_bil
  open Binop
  open Unop
  open Exp
  open Stmt
  open Cast


  (** substitutes let bound variables with theirs definitions.
      May change the semantics in general, as the function will
      copy expressions that are non-generative. However,
      the semantics is preserved wrt to the official BIL semantics,
      that states that all expressions have no side effects.
  *)
  let reduce_let exp =
    let rec (/) d x = match x with
      | Load (x,y,e,s) -> Load (d/x, d/y, e, s)
      | Store (x,y,z,e,s) -> Store (d/x, d/y, d/z, e, s)
      | BinOp (b,x,y) -> BinOp (b, d/x, d/y)
      | UnOp (o,x) -> UnOp (o, d/x)
      | Int _ | Unknown _ as x -> x
      | Cast (c,n,x) -> Cast (c,n,d/x)
      | Ite (x,y,z) -> Ite (d/x, d/y, d/z)
      | Extract (n,m,x) -> Extract (n,m, d/x)
      | Concat (x,y) -> Concat (d/x, d/y)
      | Let (v,x,y) -> ((v,d/x)::d)/y
      | Var x as var -> match List.Assoc.find ~equal:Var.equal d x with
        | None -> var
        | Some exp -> exp in
    []/exp


  (** [nth n x] projects [n]'th byte from a word.
      requires that a word is at least [n] bytes long.*)
  let nth n x = Exp.Extract ((n+1) * 8 - 1, n * 8, x)


  (* we don't need a full-fledged type inference here.
     requires: well-typed exp *)
  let infer_addr_size exp =
    let open Exp in
    let rec infer = function
      | Var v -> Var.typ v
      | Store (m,_,_,_,_) -> infer m
      | Ite (_,x,y) -> both x y
      | _ -> invalid_arg "type error"
    and both x y =
      match infer x, infer y with
      | t1,t2 when Type.(t1 = t2) -> t1
      | _ -> invalid_arg "type error" in
    match infer exp with
    | Type.Mem (s,_) -> s
    | Type.Imm _ -> invalid_arg "type error"



  let make_succ m =
    let int n =
      let size = infer_addr_size m in
      Exp.Int (Word.of_int ~width:(Size.in_bits size) n) in
    let sum a n = Exp.BinOp (Binop.PLUS, a,int n) in
    sum

  (* rewrite_store_little
     Store(m,a,x,e,s) =>
     Store(..(Store(Store(m,a,x[0],e,1),a+1,x[1],e,1))..,a+s-1,x[s-1],e,1)

     mem with [a+0]:u8 <- extract:7:0[x]
         with [a+1]:u8 <- extract:15:8[x]
         with [a+2]:u8 <- extract:23:16[x]
         with [a+3]:u8 <- extract:31:24[x]

  *)
  let expand_store m a x e s =
    let (++) = make_succ m in
    let n = Size.in_bytes s in
    let nth i = if e = BigEndian then nth (n-i-1) else nth i in
    let rec expand i =
      if i >= 0
      then Exp.Store(expand (i-1),(a++i),nth i x,LittleEndian,`r8)
      else m in
    expand (n-1)

  (* x[a,el]:n => x[a+n-1] @ ... @ x[a] x[a,be]:n => x[a] @ ... @
     x[a+n-1]

     This operation duplicates the address expression, this may break
     semantics if this expression is non-generative.

     Special care should be taken if the expression contains store
     operations, that has an effect that may interfere with the result
     of the load operation.
  *)
  let expand_load m a e s =
    let (++) = make_succ m in
    let cat x y = if e = LittleEndian
      then Exp.Concat (y,x)
      else Exp.Concat (x,y) in
    let load a = Exp.Load (m,a,e,`r8) in
    let rec expand a i =
      if i > 1
      then cat (load a) (expand (a++1) (i-1))
      else load a in
    expand a (Size.in_bytes s)

  let expand_memory = map_exp @@ object
      inherit exp_mapper as super
      method! map_load ~mem ~addr e s =
        let mem =  super#map_exp mem in
        let addr = super#map_exp addr in
        expand_load mem addr e s
      method! map_store ~mem ~addr ~exp:x e s =
        let mem =  super#map_exp mem in
        let addr = super#map_exp addr in
        let x = super#map_exp x in
        expand_store mem addr x e s
    end

  (* ensures: no-lets, one-byte-stores, one-byte-loads.
     This is the first step of normalization. The full normalization,
     e.g., remove ite and hoisting storages can be only done on the
     BIL level.

     requires:
       - generative-load-addr,
       - generative-store-mem,
       - generative-store-val,
       - generative-let-value

  *)
  let normalize_exp x = expand_memory (reduce_let x)

  type assume = Assume of (exp * bool)


  (* assume that c is equal to yes in the statement and rewrite all
     Ite exressions accordingly.
  *)
  let assume_exp (Assume (c,yes)) =
    map_exp @@ object inherit bil_mapper as super
      method! map_ite ~cond ~yes:x ~no:y =
        if Bap_exp.equal c cond
        then if yes then x else y
        else super#map_ite ~cond ~yes:x ~no:y
    end


  (* extends assume_exp to the BIL level  *)
  let assume assm stmt =
    let exp = assume_exp assm in
    let rec apply = function
      | CpuExn _ | Special _ as s -> s
      | Move (v,xs) -> Move (v, exp xs)
      | Jmp x -> Jmp (exp x)
      | If (c,xs,ys) -> If (exp c, map xs, map ys)
      | While (c,xs) -> While (exp c, map xs)
    and map = List.map ~f:apply in
    apply stmt

  let with_true v x = assume (Assume (v,true)) x
  let with_false v x = assume (Assume (v,false)) x

  let find_cond_in_exp = find @@ object
      inherit [exp] exp_finder
      method! enter_ite ~cond ~yes:_ ~no:_ {return} =
        return (Some cond)
    end

  let find_cond_in_stmt = find_stmt @@ object
      inherit [exp] bil_finder
      method! enter_ite ~cond ~yes:_ ~no:_ {return} =
        return (Some cond)
    end



  let find_non_generative_mem = find @@ object
      inherit [exp] exp_finder
      method! enter_load ~mem ~addr:_ _e _s ({return} as continue) =
        match mem with
        | Var _ -> continue
        | exp -> return (Some exp) (* always non-generative in well-typed programs *)
    end

  type 'a hoister = stmt list * 'a

  (* the expression normalization procedure will duplicate
     expressions at certain points, specified below:

     * Let (_,x,_)
     * Store (_,a,x,_,_)
     * Load (m,a,_,_)

     Those expressions (m,a,x) shall be generative, i.e., the must not
     have any observable by other expressions side-effects.

     Since signals and register reads are not observable by
     expressions, they could be ignored. However, load and store
     operations are observable, thus expressions that perform them can
     not be duplicated or rearranged in arbitrary order.

     The pass below hoists expressions from the specified points if
     the expressions perform load or store operation into standalone
     BIL statements. The order and the number of operations is
     preserved.
  *)
  let hoist_non_generative_expressions =
    let (>>=) (a,x) f =
      let b,x = f x in
      (a @ b, x) in
    let return x = ([],x) in
    let hoist x : exp hoister =
      let eff = Eff.compute x in
      if Eff.loads eff || Eff.stores eff then
        let t = Type_infer.infer_exn x in
        let v = Var.create ~is_virtual:true ~fresh:true "h" t in
        [Move (v,x)], Var v
      else return x in
    let (>>|) (hoisted,x) f = hoisted @ [f x] in
    let rec exp : exp -> exp hoister = function
      | Int _ | Var _ | Unknown _ as x -> return x
      | UnOp (op,x) -> exp x >>= fun x -> return (UnOp (op, x))
      | BinOp (op,x,y) ->
        exp x >>= fun x ->
        exp y >>= fun y ->
        return (BinOp (op,x,y))
      | Load (m,a,e,s) ->
        hoist m >>= fun m ->
        hoist a >>= fun a ->
        return (Load (m,a,e,s))
      | Store (m,a,v,e,s) ->
        exp m >>= fun m ->
        exp a >>= fun a ->
        exp v >>= fun v ->
        hoist a >>= fun a ->
        hoist v >>= fun v ->
        return (Store (m,a,v,e,s))
      | Cast (t,s,x) ->
        exp x >>= fun x ->
        return (Cast (t,s,x))
      | Let (v,x,y) ->
        exp x >>= fun x ->
        exp y >>= fun y ->
        hoist x >>= fun x ->
        return (Let (v,x,y))
      | Ite (x,y,z) ->
        exp x >>= fun x ->
        exp y >>= fun y ->
        exp z >>= fun z ->
        hoist x >>= fun x ->
        return (Ite (x,y,z))
      | Extract (n,m,x) ->
        exp x >>= fun x ->
        return (Extract (n,m,x))
      | Concat (x,y) ->
        exp x >>= fun x ->
        exp y >>= fun y ->
        return (Concat (x,y)) in
    let rec stmt : stmt -> stmt list = function
      | Special _ | CpuExn _ as s -> [s]
      | Move (v,x) -> exp x >>| fun x ->
        Move (v,x)
      | Jmp x -> exp x >>| fun x ->
        Jmp x
      | If (c,xs,ys) -> exp c >>| fun c ->
        If (c,stmts xs, stmts ys)
      | While (c,xs) -> exp c >>| fun c ->
        While (c,stmts xs)
    and stmts = List.concat_map ~f:stmt in
    stmt


  let rec memory_of_store = function
    | Store (Var mem,_,_,_,_) -> mem
    | Store (mem,_,_,_,_) -> memory_of_store mem
    | _ -> invalid_arg "memory_of_store: applied non to a store"

  (* hoists non-trivial while conditions and load memory arguments.

     while-cond-hoisting
     ===================

     a non-trivial while condition is a condition that contans an
     if-then-else expression, for example

        while (c ? x : y) prog;

     since the condition is evaluated implicitly as a part of the prog
     body we can apply a regular rewriting to get rid of the
     if-then-else expression, e.g. the following program modification
     is incorrect,

        if (c)
         while (x) prog;
        else
         while (y) prog;


     Thus, when we see that condition we hoist it to two definitions:

        h = c ? x : y;
        while (h) {prog; h = c ? x : y}

     So that the if-then-else elimination pass will yield the following
     program:

       if (c)
         h := x;
       else
         h := y;

       while (h) {
         prog;
         if (c)
           h := x;
         else
           h := y;
       }


     load-memory-argument
     ====================

     We want to ensure that all load expressions refer to memory as a
     variable, not as arbitrary store expressions. This will
     essentialy disallow memory operations that will not have any
     observable side effects, e.g., the [(mem [a] <- x)[a]]
     expression creates an anonymous memory storage on fly, stores a
     value in it, and then discards it without commiting the effect of
     writting.

     The transformation will force the memory-commit operation, so
     that if there is any memory load with a non-trivial (i.e., not a
     variable) memory argument, then this argument is hoisted as a
     seprate move instruction. This is not always correct, e.g.,


         z := (m[a]<-x)[a] + (m[a]<-y)[a]


     is not the same as

         m := m[a]<-x
         m := m[a]<-y
         z := m[a] + m[a]


     But the lifter shouldn't produce such program on a first hand, as
     this program wouldn't have a physical representation.
  *)


  let new_hoisted_var () =
    Var.create ~is_virtual:true ~fresh:true "tmp" (Type.Imm 1)


  (** [hoist_whiles ~has_anomality : bil -> bil] generic while
      condition hoister.  hoists an offending expression from a
      conditional of a while statement into two separate assignments,
      e.g.,

      [while (c) prog] if [has_anomality c] is transformed to
      [v := c; while (v) {prog; v := c}]

      [has_anomality c : bool] is applied to the conditional
      expression of a while statement and should return [true] if a
      conditional must be hoisted.

      requires: [has_anomality (Var _) = false]
      provides: [has_anomality c] is false forall while conditionals.
  *)
  let hoist_whiles ~has_anomality =
    map @@ object
      inherit bil_mapper as super
      method! map_while ~cond prog =
        let prog = super#run prog in
        if has_anomality cond then
          let v = new_hoisted_var () in
          let mv = Move (v,cond) in
          [mv; While (Var v, prog @ [mv])]
        else [While (cond, prog)]
    end

  let not_allowed_in_conditional cond =
    Option.is_some (find_cond_in_exp cond) ||
    Option.is_some (find_non_generative_mem cond)




  (* ensures: all while conditions are free from:
      - ite expressions;
      - store operations.

     Note the latter sounds more strong then the implementation, but
     it is true, as in a well-typed program a conditional must has
     type bool_t, and thus if the conditional contains a store
     operation, the the operation will be enclosed with the load, thus
     any store in a conditional automatically triggers the store in
     while condition. The same reasoning can be applied to all Jmp
     statements without further ado, and to all Move statements that
     has Imm type variable.*)
  let normalize_while_conditionals =
    hoist_whiles ~has_anomality:not_allowed_in_conditional


  let normalize_if_conditionals =
    map @@ object(self)
      inherit bil_mapper
      method! map_if ~cond ~yes ~no =
        let yes = self#run yes and no = self#run no in
        if not_allowed_in_conditional cond
        then
          let v = new_hoisted_var () in
          [Move (v,cond); If (Var v,yes,no)]
        else [If (cond,yes,no)]
    end

  let normalize_conditionals = Fn.compose
      normalize_while_conditionals
      normalize_if_conditionals

  (* requires: hoisted-while-cond.

     Splits move and jmp instructions that contain ite expressions
     into an if statement with two branches, e.g.,

        x := c?y:z;

     is rewritten into

       if (c)
         x := y;
       else
         x := z;
  *)
  let rec split_ite bil = map (object(self)
      inherit bil_mapper
      method! map_move v e = self#split @@ Move (v,e)
      method! map_jmp e = self#split @@ Jmp (e)
      method private split stmt = match find_cond_in_stmt stmt with
        | None -> [stmt]
        | Some x -> split_ite [If (x, [with_true x stmt], [with_false x stmt])]
    end) bil

  let rec hoist_stores bil = map (object(self)
      inherit bil_mapper
      method! map_move v e = self#hoist e @@ Move (v,e)
      method! map_jmp e = self#hoist e @@ Jmp e
      method private hoist e stmt = match find_non_generative_mem e with
        | None -> [stmt]
        | Some store ->
          let v = memory_of_store store in
          hoist_stores (Move (v,store) :: substitute store (Var v) [stmt])
    end) bil

  let bil ?normalize_exp:(ne=false) xs =
    let normalize_exp = if ne then normalize_exp else ident in
    let rec run xs =
      List.concat_map ~f:hoist_non_generative_expressions xs |>
      normalize_conditionals |>
      List.map ~f:(function
          | Move (v,x) -> Move (v, normalize_exp x)
          | Jmp x -> Jmp (normalize_exp x)
          | If (c,xs,ys) -> If (normalize_exp c, run xs, run ys)
          | While (c,xs) -> While (normalize_exp c, run xs)
          | Special _  | CpuExn _ as s -> s) |>
      hoist_stores |>
      split_ite in
    run xs
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
