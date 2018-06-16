open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor
open Exp
open Stmt

module Eff = Bap_eff
module Size = Bap_size
module Var = Bap_var
module Word = Bitvector

let map m = m#run
let map_exp m = m#map_exp
let find finder ss : 'a option = finder#find ss
let find_stmt f s = find f [s]

class rewriter x y = object
  inherit bil_mapper as super
  method! map_exp z =
    let z = super#map_exp z in
    if Bap_exp.(z = x) then y else z
end


let substitute x y = (new rewriter x y)#run

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
      let t = Bap_types_infer.infer_exn x in
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
