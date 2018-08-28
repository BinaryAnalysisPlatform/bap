open Core_kernel.Std
open Bap.Std
open Regular.Std
open Format
include Self()

module Digest = Data.Cache.Digest
module O = Optimization_data

type level = int

let can_touch_physicals level = level > 2
let can_touch_flags level = level > 1

let is_optimization_allowed is_flag level var =
  (Var.is_physical var && can_touch_physicals level) ||
  (is_flag var && can_touch_flags level) ||
  Var.is_virtual var

let def_use_collector = object
  inherit [Var.Set.t * Var.Set.t] Term.visitor

  method! enter_def t (defs,uses) =
    Set.add defs (Def.lhs t), Set.union uses (Def.free_vars t)

  method! enter_phi t (defs,uses) =
    Set.add defs (Phi.lhs t), Set.union uses (Phi.free_vars t)

  method! enter_jmp t (defs,uses) =
    defs, Set.union uses (Jmp.free_vars t)
end

let computed_def_use sub =
  def_use_collector#visit_sub sub (Var.Set.empty,Var.Set.empty)

let compute_dead can_touch protected sub =
  let defs,uses = computed_def_use sub in
  let dead = Set.diff defs uses in
  let live v = not (Set.mem dead v) in
  (object inherit [Tid.Set.t] Term.visitor
    method! enter_def t dead =
      let v = Def.lhs t in
      if not (can_touch v) || Set.mem protected (Var.base v) || live v
      then dead
      else Set.add dead (Term.tid t)
  end)#visit_sub sub Tid.Set.empty

let is_alive dead t = not (Set.mem dead (Term.tid t))
let live_phi dead blk = Term.filter phi_t ~f:(is_alive dead) blk

let live_def can_touch dead blk =
  Term.filter def_t blk ~f:(fun d ->
      not (can_touch (Def.lhs d)) || is_alive dead d)

let rec substitute vars exp =
  let substituter = object
    inherit Exp.mapper as super
    method! map_let var ~exp ~body =
      let exp = super#map_exp exp in
      let body = substitute (Map.remove vars var) body in
      Bil.let_ var exp body
    method! map_var v = match Map.find vars v with
      | None -> Bil.var v
      | Some e -> e
  end in
  substituter#map_exp exp |> Exp.fold_consts

let equal_kinds j j' = compare_jmp_kind (Jmp.kind j) (Jmp.kind j') = 0

let substitute sub vars =
  let def d =
    let rhs = substitute vars (Def.rhs d) in
    if Exp.(Def.rhs d <> rhs) then
      O.mark_updated (Def.with_rhs d rhs)
    else d in
  let jmp j =
    let j' = Jmp.map_exp j ~f:(substitute vars) in
    if Exp.(Jmp.cond j <> Jmp.cond j') || not (equal_kinds j j')
    then O.mark_updated j'
    else j in
  Term.map blk_t sub ~f:(Blk.map_elts ~def ~jmp)

(* A simple constant propagation. Note, the input is required to be in SSA. *)
let propagate_consts can_touch sub =
  Seq.fold (Term.enum blk_t sub) ~init:Var.Map.empty
    ~f:(fun vars b ->
        Seq.fold (Term.enum def_t b) ~init:vars
          ~f:(fun vars d ->
              let v = Def.lhs d in
              if can_touch v then
                match Def.rhs d with
                | Bil.Unknown _ | Bil.Int _ as exp ->
                  Map.add vars ~key:v ~data:exp
                | _ -> vars
              else vars)) |>
  substitute sub

let clean can_touch dead sub =
  Term.map blk_t sub ~f:(fun b -> live_def can_touch dead b |> live_phi dead)

let is_flag arch =
  let module T = (val (target_of_arch arch)) in
  T.CPU.is_flag

let free_vars prog =
  report_progress ~note:"free-vars" ();
  let (++) = Set.union in
  let collect cls t ~f =
    Seq.fold (Term.enum cls t) ~init:Var.Set.empty
      ~f:(fun acc x -> acc ++ f x) in
  let sub_free sub = Sub.free_vars sub |> Set.filter ~f:Var.is_physical in
  let sub_args sub =
    collect arg_t sub ~f:(fun arg -> Exp.free_vars (Arg.rhs arg)) in
  collect sub_t prog ~f:(fun sub -> sub_args sub ++ sub_free sub)

let process_sub free can_touch sub =
  let rec loop dead s =
    let s = propagate_consts can_touch s in
    let dead' = compute_dead can_touch free s in
    let dead = Set.union dead dead' in
    if Set.is_empty dead' then s, dead
    else loop dead (clean can_touch dead' s) in
  let sub', dead = loop Tid.Set.empty (Sub.ssa sub) in
  O.create dead sub'

let digest_of_sub sub level =
  let digest =
    (object
      inherit [Digest.t] Term.visitor
      method! enter_arg t dst = Digest.add dst "%a" Arg.pp t
      method! enter_def t dst = Digest.add dst "%a" Def.pp t
      method! enter_jmp t dst = Digest.add dst "%a" Jmp.pp t
    end)#visit_sub sub
      (Digest.create ~namespace:"optimization") in
  let digest = Digest.add digest "%s" (Sub.name sub) in
  Digest.add digest "%s" (string_of_int level)

let run level proj =
  let arch = Project.arch proj in
  let can_touch = is_optimization_allowed (is_flag arch) level in
  let prog = Project.program proj in
  let free = free_vars prog in
  Project.with_program proj @@
  Term.map sub_t prog ~f:(fun sub ->
      let digest = digest_of_sub sub level in
      let data = match O.Cache.load digest with
        | Some data -> data
        | None ->
          let data = process_sub free can_touch sub in
          O.Cache.save digest data;
          data in
      O.apply sub data)

let () =
  Config.manpage [
    `S "SYNOPSIS";
    `Pre "
     $(b,--)$(mname)
";
    `S "DESCRIPTION";

    `P "An autorun pass that conservatively removes dead code. The
  removed dead code is usually produced by a lifter, though it might
  be possible that a binary indeed contains a dead code. The algorithm
  doesn't remove variables that are stored in memory, only registers
  are considered";

    `S "ALGORITHM";

    `P "To make analysis inter procedural, we first compute an
  over-approximation of a set of variables that are used to pass data
  between functions. This is just a set of all free variables in all
  functions. Variables that belong to this set will never be
  removed. Thus if a function $(b,f) uses a variable $(b,x), then we
  consider, that any other function may somehow call this function
  $(b,f), so we assume that it is used implicitly. External functions
  must obey the ABI, thus a set of all variables used to pass
  arguments to external functions is also added to the protected
  set. Once the protected set is computed, we compute def/use sets for
  each subroutine and remove all definitions that are not
  used. Internally, we translate a program into the SSA form, though
  it will not be seen outside";

    `S "DEPENDENCIES";
    `P "$(b,bap-plugin-api)(1)";

    `S "SEE ALSO";
    `P "$(b,bap-plugin-api)(1), $(b,bap-plugin-ssa)(1)";
  ];
  let level =
    let doc =
      "Specifies the optimization level. The higher the value the more
       aggressive (and less safe) optimizations are applied. On level
       1, we optimize only the synthetic code that was generated by
       the lifter. Since such code can't leave a scope of instruction
       it is not affected by the imprecision of a control flow graph.
       On level 2, we also move and optimize processor flags. This
       removes a significant amount of code and simplifies the program
       and is a fair compromise between safety and performance.
       (Since flags are rarely used non-locally).
       Finally, on level 3 we extend our analysis to all variables." in

    Config.(param int ~default:2 ~doc "level") in

  Config.when_ready (fun {Config.get=(!)} ->
      Project.register_pass ~deps:["api"] ~autorun:true (run !level))
