open Core_kernel.Std
open Monads.Std
open Bap_common
open Bap_bil
open Bap_visitor

module Word = Bitvector
module Var = Bap_var
module Size = Bap_size

let find finder ss : 'a option = finder#find ss
let exists finder ss = Option.is_some (finder#find ss)
let iter (visitor : unit #bil_visitor) ss = visitor#run ss ()
let fold (visitor : 'a #bil_visitor) ~init ss = visitor#run ss init
let map m = m#run

let is_assigned ?(strict=false) x = exists (object(self)
    inherit [unit] bil_finder
    method! enter_move y _ cc =
      if Bap_var.(x = y) && not(strict && under_condition)
      then cc.return (Some ());
      cc
  end)

class exp_reference_finder x = object(self)
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
    if Int.(bitwidth u = bitwidth v) && not (is_shift op)
    then failwith "type error";
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
end

module Simpl = struct
  open Bap_bil
  open Binop
  open Unop
  open Exp
  open Stmt

  module Size = Bap_size
  module Var = Bap_var

  let rec infer_type = function
    | Var v -> Var.typ v
    | Int x -> Type.Imm (Word.bitwidth x)
    | Unknown (_,t) -> t
    | Load (_,_,_,s) -> Type.Imm (Size.in_bits s)
    | Cast (_,s,_) -> Type.Imm s
    | Store (m,_,_,_,_) -> infer_type m
    | BinOp ((LT|LE|EQ|NEQ|SLT|SLE),_,_) -> Type.Imm 1
    | BinOp (_,x,y) -> unify x y
    | UnOp (_,x) -> infer_type x
    | Let (_,_,x) -> infer_type x
    | Ite (_,x,y) -> unify x y
    | Extract (hi,lo,_) -> Type.Imm (hi - lo + 1)
    | Concat (x,y) -> match infer_type x, infer_type y with
      | Type.Imm s, Type.Imm t -> Type.Imm (s+t)
      | _ -> failwith "concat is not applicable to mem"
  and unify x y =
    let t1 = infer_type x and t2 = infer_type y in
    if t1 = t2 then t1 else failwith "type error"

  let width x = match infer_type x with
    | Type.Imm x -> x
    | Type.Mem _ -> failwith "width is not for memory"

  let zero width = Int (Word.zero width)
  let ones width = Int (Word.ones width)
  let is0 = Word.is_zero
  and is1 = Word.is_one
  let ism1 x = Word.is_zero (Word.lnot x)
  let nothing _ = false


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
    and cast t s x = match t with
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
    and ite c x y = match bits x, bits y with
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
     the expression, that have any effects are preserved and are not
     reordered.

     Note, the function inherits preconditions from the [Nz.bits].
  *)

  let rec eff = function
    | Load _ | Store _ -> true
    | BinOp ((DIVIDE|SDIVIDE|MOD|SMOD),x,y) -> eff x || div y
    | BinOp (_,x,y) -> eff x || eff y
    | UnOp (_,x) -> eff x
    | Var _ | Int _ | Unknown _ -> false
    | Cast (_,_,x) -> eff x
    | Let (_,x,y) -> eff x || eff y
    | Ite (x,y,z) -> eff x || eff y || eff z
    | Extract (_,_,x) -> eff x
    | Concat (x,y) -> eff x || eff y
  and div y = match Nz.bits y with
    | Nz.Maybe | Nz.Empty -> true
    | _ -> false

  let rec exp = function
    | Load (m,a,e,s) -> Load (exp m, exp a, e, s)
    | Store (m,a,v,e,s) -> Store (exp m, exp a, exp v, e, s)
    | BinOp (op,x,y) -> binop op x y
    | UnOp (op,x) -> unop op x
    | Var _ | Int _  | Unknown (_,_)as const -> const
    | Cast (t,s,x) -> Cast (t,s,exp x)
    | Let (v,x,y) -> Let (v, exp x, exp y)
    | Ite (x,y,z) -> Ite (exp x, exp y, exp z)
    | Extract (h,l,x) -> Extract (h,l, exp x)
    | Concat (x,y) -> concat x y
  and concat x y = match x, y with
    | Int x, Int y -> Int (Word.concat x y)
    | _ -> Concat (exp x, exp y)
  and unop op x = match x with
    | UnOp(op,Int x) -> Int (Apply.unop op x)
    | UnOp(op',x) when op = op' -> exp x
    | _ -> UnOp(op, exp x)
  and binop op x y =
    let width = match unify x y with
      | Type.Imm s -> s
      | Type.Mem _ -> failwith "binop" in
    let return op x y = BinOp(op,x,y) in
    let int f = function Int x -> f x | _ -> false in
    let is0 = int is0 and is1 = int is1 and ism1 = int ism1 in
    let (=) x y = compare_exp x y = 0 && not (eff x) in
    match op, exp x, exp y with
    | op, Int x, Int y -> Int (Apply.binop op x y)
    | PLUS,x,y  when is0 x -> y
    | PLUS,x,y  when is0 y -> x
    | MINUS,x,y when is0 x -> UnOp(NEG,y)
    | MINUS,x,y when is0 y -> x
    | MINUS,x,y when x = y -> zero width
    | TIMES,x,y when is0 x -> x
    | TIMES,x,y when is0 y -> y
    | TIMES,x,y when is1 x -> y
    | TIMES,x,y when is1 y -> x
    | (DIVIDE|SDIVIDE),x,y when is0 y -> return op x y
    | (DIVIDE|SDIVIDE),x,y when is1 y -> x
    | (MOD|SMOD),x,y when is1 y -> zero width
    | (LSHIFT|RSHIFT|ARSHIFT),x,y when is0 y -> x
    | (LSHIFT|RSHIFT|ARSHIFT),x,y when is0 x -> x
    | (LSHIFT|RSHIFT|ARSHIFT),x,y when ism1 x -> x
    | AND,x,y when is0 x -> x
    | AND,x,y when is0 y -> y
    | AND,x,y when ism1 x -> y
    | AND,x,y when ism1 y -> x
    | AND,x,y when x = y -> x
    | OR,x,y  when is0 x -> y
    | OR,x,y  when is0 y -> x
    | OR,x,y  when ism1 x -> x
    | OR,x,y  when ism1 y -> y
    | OR,x,y  when x = y -> x
    | XOR,x,y when x = y -> zero width
    | XOR,x,y when is0 x -> y
    | XOR,x,y when is0 y -> y
    | EQ,x,y  when x = y -> Int Word.b1
    | NEQ,x,y when x = y -> Int Word.b0
    | (LT|SLT), x, y when x = y -> Int Word.b0
    | _ -> return op x y

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
  and bil = List.concat_map ~f:stmt
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

  let simplify = List.map ~f:fixpoint [
      prune;
      normalize_negatives;
      Simpl.bil;
    ] |> List.reduce_exn ~f:Fn.compose |> fixpoint

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
  let fold_consts = Simpl.exp
  let fixpoint = fix compare_exp

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
      | Var x as var -> match List.Assoc.find d x with
        | None -> var
        | Some exp -> exp in
    []/exp

  let nth n x =
    let hi = (n+1) * 8 - 1 in
    let lo = n * 8 in
    Exp.Extract (hi,lo,x)

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
    let rec rewrite i =
      if i >= 0
      then Exp.Store(rewrite (i-1),(a++i),nth i x,LittleEndian,`r8)
      else m in
    rewrite (n-1)

  let expand_load m a e s =
    let (++) = make_succ m in
    let cat x y = if e = LittleEndian
      then Exp.Concat (y,x)
      else Exp.Concat (x,y) in
    let load a = Exp.Load (m,a,e,`r8) in
    let rec rewrite a i =
      if i > 1
      then cat (load a) (rewrite (a++1) (i-1))
      else load a in
    rewrite a (Size.in_bytes s)

  let normalize exp = Simpl.exp @@ match reduce_let exp with
    | Load (m,a,e,s) -> expand_load m a e s
    | Store (m,a,x,e,s) -> expand_store m a x e s
    | exp -> exp

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
end

let free_vars = Stmt.bil_free_vars
let fold_consts = Simpl.bil
