open Core_kernel.Std
open Bap_common
open Bap_bil
open Exp

module Word = Bitvector

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

let is0 = Word.is_zero and is1 = Word.is_one
let ism1 x = Word.is_zero (Word.lnot x)

let width x = match Bap_types_infer.infer_exn x with
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

let is_subset x of_ = Set.is_subset x ~of_
