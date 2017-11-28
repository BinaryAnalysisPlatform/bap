open Bap.Std
open Core_kernel
open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Value = Bap_primus_value
module Loc = Bap_primus_lisp_loc
module Program = Bap_primus_lisp_program

open Bap_primus_lisp_attributes



type stage = Id.Set.t
type resolution = {
  stage1 : stage; (* definitions with the given name *)
  stage2 : stage; (* definitions applicable to the ctxt *)
  stage3 : stage; (* lower bounds of all definitions *)
  stage4 : stage; (* infinum *)
}

type ('t,'a,'b) resolver =
  Program.t -> 't Program.item -> string -> 'a -> ('t Def.t * 'b,resolution) result option

type exn += Failed of string * Context.t * resolution

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


let overload_defun typechecks args s3 =
  let open Option in
  List.filter_map s3 ~f:(fun def ->
      List.zip (Def.Func.args def) args >>= fun bs ->
      if List.for_all ~f:(fun (v,x) ->
          typechecks v.typ x) bs
      then Some (def,bs) else None)

let overload_primitive s3 = List.map s3 ~f:(fun s -> s,())

let locs defs =
  List.map defs ~f:(fun def -> def.id) |> Id.Set.of_list

let run namespace overload prog item (name : string) =
  let ctxts = Program.context prog in
  let defs = Program.get prog item in
  let s1 = stage1 namespace defs name in
  let s2 = stage2 ctxts s1 in
  let s3 = stage3 s2 in
  let s4 = stage4 s3 in
  match overload s4 with
  | [f] -> Some (Ok f)
  | _ -> match s1 with
    | [] -> None
    | _ ->  Some( Error {
        stage1 = locs s1;
        stage2 = locs s2;
        stage3 = locs s3;
        stage4 = locs s4;
      })

let extern typechecks prog item name args =
  run externs (overload_defun typechecks args) prog item name

let defun typechecks prog item name args =
  run interns (overload_defun typechecks args) prog item name

let macro prog item name code =
  run interns (overload_macro code) prog item name

let primitive prog item name () =
  run interns overload_primitive prog item name

let subst prog item name () =
  run interns overload_primitive prog item name

let const = subst


let pp_resolution ppf res =
  Format.fprintf ppf "ambiguous definitions"
