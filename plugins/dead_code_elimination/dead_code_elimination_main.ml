open Core_kernel.Std
open Bap.Std
open Format
include Self()

let union ~init ~f =
  Seq.fold ~init ~f:(fun acc x ->
      Set.union acc (f x))

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

let sub_args sub =
  Term.enum arg_t sub |>
  union ~init:Var.Set.empty ~f:(fun arg -> Exp.free_vars (Arg.rhs arg))
let free_vars sub = Set.union (Sub.free_vars sub) (sub_args sub)

let compute_dead protected sub =
  let defs,uses = computed_def_use sub in
  let dead = Set.diff defs uses in
  let live v = not (Set.mem dead v) in
  (object inherit [Tid.Set.t] Term.visitor
      method! enter_def t dead =
        let v = Def.lhs t in
        if Set.mem protected (Var.base v) || live v
        then dead
        else (Set.add dead (Term.tid t))
  end)#visit_sub sub Tid.Set.empty


let process proj =
  info "removing dead code";
  let prog = Project.program proj in
  let subs = Term.enum sub_t prog |> Seq.map ~f:Sub.ssa in
  let free = union subs ~init:Var.Set.empty ~f:free_vars in
  let dead = union subs ~init:Tid.Set.empty ~f:(compute_dead free) in
  let live t = not (Set.mem dead (Term.tid t)) in
  Term.map sub_t prog ~f:(Term.map blk_t ~f:(Term.filter def_t ~f:live)) |>
  Project.with_program proj
let () = Config.when_ready (fun _ ->
    Project.register_pass ~deps:["api"] ~autorun:true process)
;;

Config.manpage [
  `S "SYNOPSIS";
  `Pre "
    $(b,--no-$mname)
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
]
