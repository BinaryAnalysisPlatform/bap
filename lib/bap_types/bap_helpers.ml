open Core_kernel.Std
open Bap_common
open Bap_bil
open Bap_visitor

module Word = Bitvector

let find_map (finder : 'a #finder) ss : 'a option = finder#find ss
let find finder ss = finder#find ss = Some ()
let iter (visitor : unit #visitor) ss = visitor#run ss ()
let fold (visitor : 'a #visitor) ~init ss = visitor#run ss init
let map m = m#run

let is_assigned ?(strict=false) x = find (object(self)
    inherit [unit] finder
    method! enter_move y _ cc =
      if Bap_var.(x = y) && not(strict && under_condition)
      then cc.return (Some ());
      cc
  end)

let is_referenced x = find (object(self)
    inherit [unit] finder
    method! enter_var y cc =
      if Bap_var.(x = y) then cc.return (Some ()); cc
  end)

let prune_unreferenced stmt =
  let rec loop ss = function
    | [] -> List.rev ss
    | Stmt.Move (x,_) as s :: xs when Bap_var.is_tmp x ->
      if is_referenced x xs then loop (s::ss) xs else loop ss xs
    | s :: xs -> loop (s::ss) xs in
  loop [] stmt

let normalize_negatives = (object inherit mapper as super
  method! map_binop op e1 e2 = match op,e2 with
    | Binop.PLUS, Exp.Int arg
      when Word.(is_negative (signed arg)) ->
      let (-) = Bap_exp.Infix.(-) in
      (e1 - Exp.Int Word.(abs (signed arg)))
    | _ -> super#map_binop op e1 e2
end)#run


include struct
  open Exp
  class constant_folder =
    object inherit mapper as super
      method! map_binop op e1 e2 =
        let open Binop in
        let zero v1 v2 = match v1,v2 with
          | Var v,_ | _, Var v -> begin match Bap_var.typ v with
              | Type.Imm width -> Int (Word.zero width)
              | Type.Mem _ -> super#map_binop op e1 e2
            end
          | _ -> super#map_binop op e1 e2 in
        match op, e1, e2 with
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
              | SLT     -> bool (signed v1 <= signed v2)
              | SLE     -> bool (signed v1 <= signed v2))
        | (PLUS|LSHIFT|RSHIFT|ARSHIFT|OR|XOR), Int v, e
        | (PLUS|MINUS|LSHIFT|RSHIFT|ARSHIFT|OR|XOR), e, Int v
          when Word.is_zero v -> e
        | TIMES,e,Int v | TIMES, Int v, e when Word.is_one v -> e
        | (TIMES|AND), e, Int v
        | (TIMES|AND), Int v, e when Word.is_zero v -> Int v
        | (OR|AND), v1, v2 when compare_exp v1 v2 = 0 -> v1
        | (XOR), v1, v2 when compare_exp v1 v2 = 0 -> zero v1 v2
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

    end
end
let fold_consts = (new constant_folder)#run


let connection_point (next : 'a -> 'a option) x : 'a option =
  let collision_point init =
    let rec loop slow = function
      | None -> None
      | Some fast when phys_equal slow fast -> Some fast
      | Some fast -> match next slow with
        | None -> assert false
        | Some slow -> match next fast with
          | None -> None
          | Some fast -> loop slow (next fast) in
    loop init (next init) in
  let convergent_point x y =
    let next p = match next p with
      | None -> invalid_arg "transformation has terminated"
      | Some p -> p in
    let rec loop x y =
      if phys_equal x y then x else loop (next x) (next y) in
    loop x y in
  match collision_point x with
  | None -> None
  | Some p -> match next p with
    | None -> Some p
    | Some p -> Some (convergent_point x p)

(* later we can provide a hashconsing, but for now a simple
   but safe implementation.
   The algorithm is adopted from A. Stepanov and P. McJones
   collision point algorithm [ISBN-10: 0-321-63537-X].
*)
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
