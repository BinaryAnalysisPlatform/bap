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

let prune_unreferenced stmt =
  let rec loop ss = function
    | [] -> List.rev ss
    | Stmt.Move (x,_) as s :: xs when Bap_var.is_tmp x ->
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

include struct
  open Exp
  class constant_folder =
    object
      inherit mapper as super
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
        | op, Int v1, Int v2 ->
          let open Bap_exp.Exp in
          let signed = Word.signed in
          let bool v = int (Word.of_bool v) in
          let open Word.Mono in
          Word.Int_exn.(match op with
              | PLUS    -> int (v1 + v2)
              | MINUS   -> int (v1 - v2)
              | TIMES   -> int (v1 * v2)
              | DIVIDE  -> int (v1 / v2)
              | SDIVIDE -> int (signed v1 / signed v2)
              | MOD     -> int (v1 mod v2)
              | SMOD    -> int (signed v1 mod signed v2)
              | LSHIFT  -> int (v1 lsl v2)
              | RSHIFT  -> int (v1 lsr v2)
              | ARSHIFT -> int (v1 asr v2)
              | AND     -> int (v1 land v2)
              | OR      -> int (v1 lor v2)
              | XOR     -> int (v1 lxor v2)
              | EQ      -> bool (v1 = v2)
              | NEQ     -> bool (v1 <> v2)
              | LT      -> bool (v1 < v2)
              | LE      -> bool (v1 <= v2)
              | SLT     -> bool (signed v1 < signed v2)
              | SLE     -> bool (signed v1 <= signed v2))
        | (PLUS|LSHIFT|RSHIFT|ARSHIFT|OR|XOR), Int v, e
        | (PLUS|MINUS|LSHIFT|RSHIFT|ARSHIFT|OR|XOR), e, Int v
          when Word.is_zero v -> e
        | (TIMES|AND),e,Int v
        | (TIMES|AND), Int v, e when Word.is_one v -> e
        | (TIMES|AND), e, Int v
        | (TIMES|AND), Int v, e when Word.is_zero v -> Int v
        | (OR|AND), v1, v2 when equal v1 v2 -> v1
        | (XOR), v1, v2 when equal v1 v2 -> zero v1 v2
        | op, Int v, e -> super#map_binop op e (Int v)
        | PLUS, BinOp (PLUS, a, Int b), Int c ->
          BinOp (PLUS, a, super#map_binop PLUS (Int b) (Int c))
        | PLUS, BinOp (MINUS, a, Int b), Int c ->
          BinOp (MINUS, a, super#map_binop MINUS (Int b) (Int c))
        | MINUS, BinOp (MINUS, a, Int b), Int c ->
          BinOp (MINUS, a, super#map_binop PLUS (Int b) (Int c))
        | _ -> super#map_binop op e1 e2

      method! map_unop op arg = match arg with
        | Int v -> Unop.(match op with
            | NEG -> Int Word.(neg v)
            | NOT -> Int Word.(lnot v))
        | _ -> super#map_unop op arg

      method! map_cast kind size arg =
        let cast kind v =
          let open Cast in match kind with
          | UNSIGNED -> Word.extract_exn ~hi:(size - 1) v
          | SIGNED   -> Word.extract_exn ~hi:(size - 1) (Word.signed v)
          | HIGH     -> Word.extract_exn ~lo:(Word.bitwidth v - size) v
          | LOW      -> Word.extract_exn ~hi:(size - 1) v in
        match arg with
        | Int v -> Int (cast kind v)
        | _ -> super#map_cast kind size arg

      method! map_let var ~exp ~body =
        match exp with
        | Int v ->
          (object inherit mapper
            method! map_var z =
              if Bap_var.(z = var) then exp else (Exp.Var z)
          end)#map_exp body
        | _ -> super#map_let var ~exp ~body

      method! map_ite ~cond ~yes ~no =
        match cond with
        | Int v -> if Word.is_zero v then no else yes
        | _ -> super#map_ite ~cond ~yes ~no

      method! map_extract ~hi ~lo = function
        | Int v -> Int (Word.extract_exn ~hi ~lo v)
        | e  -> super#map_extract ~hi ~lo e

      method! map_concat e1 e2 = match e1,e2 with
        | Int v1, Int v2 -> Int (Word.concat v1 v2)
        | _ -> super#map_concat e1 e2

      method! map_if ~cond ~yes ~no = match cond with
        | Int v -> if Word.is_zero v then no else yes
        | _ -> super#map_if ~cond ~yes ~no

      method! map_while ~cond bil = match cond with
        | Int v -> if Word.is_zero v then [] else bil
        | _ -> super#map_while ~cond bil

      method! map_load ~mem ~addr:la le ls =
        let loader = object
          inherit [exp] finder
          method! enter_store ~mem ~addr:sa ~exp:r se ss find =
            if compare_exp la sa = 0 then
              if ls = ss then find.return (Some r)
              else if ls > ss || se <> le then find.return None
              else
                let sz = Bap_size.(to_bits ss - to_bits ls) in
                if se = LittleEndian
                then find.return @@
                  Some (Exp.Cast (Cast.LOW, sz,  r))
                else find.return @@
                  Some (Exp.Cast (Cast.HIGH, sz, r))
            else find
        end in

        let result =
          with_return (fun r -> ignore(loader#visit_exp mem r); None) in
        match result with
        | None -> super#map_load ~mem ~addr:la le ls
        | Some exp -> exp

      method! map_store ~mem ~addr:na ~exp:nval ne ns =
        let found = ref false in
        let mem = (object
          inherit mapper as super
          method! map_store ~mem ~addr:pa ~exp:pval pe ps =
            if compare_exp pa na = 0 then begin
              found := true;
              if ns >= ps then Exp.Store (mem,nval,na,ne,ns)
              else
                let cast = if pe = LittleEndian
                  then Cast.LOW else Cast.HIGH in
                let sz = Bap_size.(to_bits ps - to_bits ns) in
                let ex =
                  Exp.Concat (nval, Exp.Cast (cast, sz, pval)) in
                Exp.Store (mem,ex,na,ne,ns)
            end
            else super#map_store ~mem ~exp:pval ~addr:pa pe ps
        end)#map_exp mem in
        if found.contents then mem
        else super#map_store ~mem ~exp:nval ~addr:na ne ns
    end
end
let fold_consts = (new constant_folder)#run


let connection_point (type t) compare (f : t -> t) x : t option =
  let collision_point init =
    let rec loop slow fast =
      if compare slow fast = 0 then Some fast
      else loop (f slow) (f (f fast)) in
    loop init (f init) in
  let convergent_point x y =
    let rec loop x y =
      if compare x y = 0 then x else loop (f x) (f y) in
    loop x y in
  match collision_point x with
  | None -> None
  | Some p -> Some (convergent_point x p)

(* later we can provide a hashconsing, but for now a simple
   but safe? implementation.
   The algorithm is adopted from A. Stepanov and P. McJones
   collision point algorithm [ISBN-10: 0-321-63537-X].
   {v
   slow = x;
   fast = f(x);
   while (fast != slow) { // slow = fn(x) ∧ fast = f2n+1(x)
     slow = f(slow); // slow = fn+1(x) ∧ fast = f2n+1(x)
     fast = f(fast); // slow = fn+1(x) ∧ fast = f2n+2(x)
     fast = f(fast); // slow = fn+1(x) ∧ fast = f2n+3(x)
   // n ← n + 1
   }
   return fast;
   v}
*)
let fix compare f x  =
  let rec loop slow fast =
    if compare slow fast = 0 then fast
    else loop (f slow) (f (f fast)) in
  loop x (f x)


let fix compare f x  =
  let rec loop slow fast =
    if compare slow fast = 0 then fast
    else
      let fast' = f fast in
      if compare fast' fast = 0 then fast
      else loop (f slow) (f fast) in
  loop x (f x)



let fixpoint = fix compare_bil

let substitute x y = (object inherit mapper as super
  method! map_exp z =
    let z = super#map_exp z in
    if Bap_exp.(z = x) then y else z
end)#run

let substitute_var x y = (object inherit mapper as super
  method! map_var z =
    match super#map_var z with
    | Exp.Var z when Bap_var.(z = x) -> y
    | z -> z
end)#run


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

  let simplify = List.map ~f:fixpoint [
      prune_unreferenced;
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
end
