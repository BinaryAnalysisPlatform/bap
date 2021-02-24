open Bap.Std
open Core_kernel
open Format
open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Value = Bap_primus_value
module Loc = Bap_primus_lisp_loc
module Program = Bap_primus_lisp_program

open Bap_primus_lisp_attributes



type stage = Loc.Set.t
type resolution = {
  constr : Context.t;
  stage1 : stage; (* definitions with the given name *)
  stage2 : stage; (* definitions applicable to the ctxt *)
  stage3 : stage; (* lower bounds of all definitions *)
  stage4 : stage; (* infinum *)
  stage5 : stage; (* overload *)
}


type ('t,'a,'b) resolver =
  Program.t -> 't Program.item -> string -> 'a ->
  ('b,resolution) result option

type ('t,'a,'b) one = ('t,'a,'t Def.t * 'b) resolver
type ('t,'a,'b) many = ('t,'a,('t Def.t * 'b) list) resolver


type exn += Failed of string * Context.t * resolution

let interns d name = String.equal (Def.name d) name
let externs def name =
  let names = Attribute.Set.get External.t (Def.attributes def) in
  Set.mem names name



(* all definitions with the given name *)
let stage1 has_name defs name =
  List.filter defs ~f:(fun def -> has_name def name)

let context def =
  Attribute.Set.get Context.t (Def.attributes def)


let compare_def d1 d2 =
  Context.(order (context d1) (context d2))

(* all definitions that satisfy the [ctxts] constraint *)
let stage2 (global : Context.t) defs =
  List.filter defs ~f:(fun def -> Context.(context def <= global))

(* returns a set of upper bounds from the given set of definitions. *)
let stage3 s2  =
  List.fold s2 ~init:[] ~f:(fun cs d -> match cs with
      | [] -> [d]
      | c :: cs -> match compare_def d c with
        | EQ | NC -> d :: c :: cs
        | LT -> c :: cs
        | GT -> [d])

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
  | x :: xs -> if List.for_all xs ~f:(fun y -> match compare_def x y with
      | EQ -> true
      | _ -> false)
    then x::xs
    else []

let overload_macro code (s3) =
  List.filter_map s3 ~f:(fun def ->
      Option.(Def.Macro.bind def code >>| fun (n,bs) -> n,def,bs)) |>
  List.sort ~compare:(fun (n,_,_) (m,_,_) -> Int.ascending n m) |> function
  | [] -> []
  | ((n,_,_) as c) :: cs -> List.filter_map (c::cs) ~f:(fun (m,d,bs) ->
      Option.some_if (n = m) (d,bs))

let all_bindings f =
  List.for_all ~f:(fun (v,x) ->
      f v.data.typ x)

let zip x y =
  match List.zip x y with
  | Ok z -> Some z
  | Unequal_lengths -> None

let overload_defun typechecks args s3 =
  let open Option in
  List.filter_map s3 ~f:(fun def ->
      zip (Def.Func.args def) args >>= fun bs ->
      if all_bindings typechecks bs
      then Some (def,bs) else None)

let zip_tail xs ys =
  let rec zip zs xs ys = match xs,ys with
    | [],[] -> zs, None
    | x,[] -> zs, Some (First x)
    | [],y -> zs, Some (Second y)
    | x :: xs, y :: ys -> zip ((x,y)::zs) xs ys in
  let zs,tail = zip [] xs ys in
  List.rev zs,tail


let overload_meth typechecks args s3 =
  List.filter_map s3 ~f:(fun m ->
      match zip_tail (Def.Meth.args m) args with
      | bs,None
      | bs, Some (Second _) when all_bindings typechecks bs ->
        Some (m,bs)
      | _ -> None)

let overload_primitive s3 = List.map s3 ~f:(fun s -> s,())

let locs prog defs =
  let src = Program.sources prog in
  List.map defs ~f:(fun def ->
      Source.loc src def.id) |> Loc.Set.of_list

let one = function
  | [x] -> Some x
  | _ -> None

let many xs = Some xs

let run choose namespace overload prog item name =
  let ctxts = Program.context prog in
  let defs = Program.get prog item in
  let s1 = stage1 namespace defs name in
  let s2 = stage2 ctxts s1 in
  let s3 = stage3 s2 in
  let s4 = stage4 s3 in
  let s5 = overload s4 in
  match choose s5 with
  | Some f -> Some (Ok f)
  | None -> match s1 with
    | [] -> None
    | _ ->  Some( Error {
        constr = ctxts;
        stage1 = locs prog s1;
        stage2 = locs prog s2;
        stage3 = locs prog s3;
        stage4 = locs prog s4;
        stage5 = locs prog (List.map s5 ~f:fst);
      })

let extern typechecks prog item name args =
  run one externs (overload_defun typechecks args) prog item name

let defun typechecks prog item name args =
  run one interns (overload_defun typechecks args) prog item name

let meth typechecks prog item name args =
  run many interns (overload_meth typechecks args) prog item name

let macro prog item name code =
  run one interns (overload_macro code) prog item name

let primitive prog item name () =
  run one interns overload_primitive prog item name

let semantics = primitive

let subst prog item name () =
  run one interns overload_primitive prog item name

let const = subst

let pp_stage ppf stage =
  if Set.is_empty stage
  then fprintf ppf "No definitions@\n"
  else Set.iter stage ~f:(fprintf ppf "%a@\n" Loc.pp)

let pp_reason ppf res =
  if Set.is_empty res.stage5
  then fprintf ppf "no suitable definitions were found.@\n"
  else fprintf ppf "several equally applicable definitions were found.@\n"

let pp_resolution ppf res =
  pp_reason ppf res;
  fprintf ppf "The following candidates were considered:@\n";
  fprintf ppf "All definitions with the given name:@\n";
  pp_stage ppf res.stage1;
  fprintf ppf "All definitions applicable to the given context:@\n";
  pp_stage ppf res.stage2;
  fprintf ppf "Definitions that are most specific to the given context:@\n";
  pp_stage ppf res.stage3;
  if Set.equal res.stage3 res.stage4
  then
    fprintf ppf "All definitions applicable to the specified arguments:@\n%a"
      pp_stage res.stage5
  else
    fprintf ppf
      "Overloading was not applied, since the above definitions \
       belong to different context classes@\n";
  fprintf ppf "Note: the definitions were considered in the \
               following context:@\n%a"
    Context.pp res.constr
