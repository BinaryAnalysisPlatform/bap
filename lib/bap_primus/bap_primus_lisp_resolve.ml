open Bap.Std
open Core_kernel
open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Value = Bap_primus_value
module Loc = Bap_primus_lisp_loc

open Bap_primus_lisp_attributes



type stage = Loc.Set.t
type resolution = {
  stage1 : stage; (* definitions with the given name *)
  stage2 : stage; (* definitions applicable to the ctxt *)
  stage3 : stage; (* lower bounds of all definitions *)
  stage4 : stage; (* infinum *)
}

type ('t,'a,'b) resolver =
  't Def.t list -> Context.t -> string -> 'a ->
  resolution * ('t Def.t * 'b) option


type exn += Failed of string * Context.t * resolution


(* let pp_stage ppf locs = *)
(*   List.iter locs ~f:(fun loc -> *)
(*       fprintf ppf "%a@\n" pp_loc loc) *)

(* let pp ppf {stage1; stage2; stage3; stage4} = *)
(*   fprintf ppf "Initial set of candidates: @\n%a@\n\ *)
(*                Candidates that satisfy current context: @\n%a@\n\ *)
(*                Most specific candidates: @\n%a@\n\ *)
(*                Candidates with compatible types and arity: @\n%a@\n\ " *)
(*     pp_stage stage1 *)
(*     pp_stage stage2 *)
(*     pp_stage stage3 *)
(*     pp_stage stage4 *)

(* let string_of_error name ctxts resolution = *)
(*   asprintf *)
(*     "no candidate for definition %s@\n\ *)
(*      evaluation context@\n%a@\n@\n%a" *)
(*     name Contexts.pp ctxts pp resolution *)

(* let () = Exn.add_printer (function *)
(*     | Failed (n,c,r) -> Some (string_of_error n c r) *)
(*     | _ -> None) *)


let interns d name = Def.name d = name
let externs def name =
  match Attribute.Set.get (Def.attributes def) External.t with
  | None -> false
  | Some names -> List.mem ~equal:String.equal names name


(* all definitions with the given name *)
let stage1 has_name defs name =
  List.filter defs ~f:(fun def -> has_name def name)

let context def =
  match Attribute.Set.get (Def.attributes def) Context.t with
  | Some cx -> cx
  | None -> Context.empty


let compare_def d1 d2 =
  Context.(compare (context d1) (context d2))

(* all definitions that satisfy the [ctxts] constraint *)
let stage2 (global : Context.t) defs =
  List.filter defs ~f:(fun def -> Context.(global <= context def))

(* returns a set of lower bounds from the given set of definitions. *)
let stage3 s2  =
  List.fold s2 ~init:[] ~f:(fun cs d -> match cs with
      | [] -> [d]
      | c :: cs -> match compare_def d c with
        | Same | Equiv -> d :: c :: cs
        | More -> c :: cs
        | Less -> [d])

(* ensures that all definitions belong to the same context class.

   if any two definitions are equivalent but not the same, then we
   drop all definitions, since if we have more than one definition at
   this stage, then the only left method of refinement is the
   overloading, and we do not want to allow the last stage to choose
   from equivalent definitions based on their type. For example

   Suppose we have two definition with the following types:

   [d1 : ((arch arm) (compiler gcc)) => (i32)]

   and

   [d2 : ((arch arm) (container elf)) => (i32 i32)]

   And we apply it two a single argument [(d x)], and the context is

   [((arch arm) (compiler gcc) (container elf) ..)], then we have two
   perfectly valid and applicable to the current context definitions,
   and we can't choose one or another based on the number of
   arguments.
*)
let stage4 = function
  | [] -> []
  | x :: xs ->
    if List.for_all xs ~f:(fun y -> compare_def x y = Same)
    then x::xs
    else []



let overload_macro code (s3) =
  List.filter_map s3 ~f:(fun def ->
      Option.(Def.Macro.bind def code >>| fun (n,bs) -> n,def,bs)) |>
  List.sort ~cmp:(fun (n,_,_) (m,_,_) -> Int.ascending n m) |> function
  | [] -> []
  | ((n,_,_) as c) :: cs -> List.filter_map (c::cs) ~f:(fun (m,d,bs) ->
      Option.some_if (n = m) (d,bs))

let typechecks arch (v,w) =
  let word_size = Size.in_bits (Arch.addr_size arch) in
  let size = Word.bitwidth (Value.to_word w) in
  match v.typ with
  | Word -> size = word_size
  | Type n -> size = n

let overload_defun arch args s3 =
  let open Option in
  List.filter_map s3 ~f:(fun def ->
      List.zip (Def.Func.args def) args >>= fun bs ->
      if List.for_all ~f:(typechecks arch) bs
      then Some (def,bs) else None)

let overload_primitive s3 = s3

let locs defs = List.map defs ~f:Def.location |> Loc.Set.of_list

let run namespace overload ctxts defs (name : string) =
  let s1 = stage1 namespace defs name in
  let s2 = stage2 ctxts s1 in
  let s3 = stage3 s2 in
  let s4 = stage4 s3 in
  let result = match overload s4 with
    | [f] -> Some f
    | _ -> None in
  {
    stage1 = locs s1;
    stage2 = locs s2;
    stage3 = locs s3;
    stage4 = locs s4;
  }, result

let extern arch defs ctxt name args =
  run externs (overload_defun arch args) ctxt defs name

let defun arch defs ctxt name args =
  run interns (overload_defun arch args) ctxt defs name

let macro defs ctxts name code =
  run interns (overload_macro code) ctxts defs name

let primitive ctxts defs name =
  run interns overload_primitive ctxts defs name
