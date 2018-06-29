open Core_kernel.Std
open Bap.Std
open Regular.Std
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

(* a simple constant propagation, that will propagate constant
   expressions from virtual variables that are assigned only once to
   the point of usage (if any).

   1. we propagate only virtuals since they are not used/clobbered by
      function calls. We can't touch real registers, as we don't know
      the calling convention. And even if we know, a malicious program
      may easily break it.

   2. instead of a full-fledges reaching definition analysis we are
      relying on a simple over-approximation: if a variable is defined
      only once, then it is never redifined. Enough for reaping the
      low hanging fruits.

   Note, the input is not required to be in SSA.
 *)
let propagate_consts sub =
  let vars = (object inherit [exp Var.Map.t] Term.visitor
      method! enter_def t vars =
        let v = Def.lhs t in
        if Var.is_virtual v
        then if Map.mem vars v
          then Map.remove vars v
          else match Def.rhs t with
            | Bil.Unknown _ | Bil.Int _ as exp ->
              Map.add vars ~key:v ~data:exp
            | _ -> vars
        else vars
  end)#visit_sub sub Var.Map.empty in
  Term.map blk_t sub ~f:(Blk.map_exp ~f:(fun exp ->
      Map.fold vars ~init:exp ~f:(fun ~key:pat ~data:rep ->
          Exp.substitute Bil.(var pat) rep)))

module X = struct

  let rec process prog =
    report_progress ~note:"ssa" ();
    let subs = Term.enum sub_t prog |> Seq.map ~f:Sub.ssa |> Seq.memoize in
    report_progress ~note:"free-vars" ();
    let free = union subs ~init:Var.Set.empty ~f:free_vars in
    report_progress ~note:"dead-vars" ();
    let dead = union subs ~init:Tid.Set.empty ~f:(compute_dead free) in
    let live t = not (Set.mem dead (Term.tid t)) in
    report_progress ~note:"clean" ();
    let clean sub = Term.map blk_t sub ~f:(Term.filter def_t ~f:live) |>
                    propagate_consts in
    report_progress ~note:"updating" ();
    if Set.is_empty dead then prog
    else
      Term.map sub_t prog ~f:clean |>
      process

end

module Y = struct

  let free_vars (_,ssa) = free_vars ssa
  let compute_dead free (_,ssa) = compute_dead free ssa
  let is_alive dead t = not (Set.mem dead (Term.tid t))
  let live_def dead blk = Term.filter def_t ~f:(is_alive dead) blk
  let live_phi dead blk = Term.filter phi_t ~f:(is_alive dead) blk

  let clean dead (sub,ssa) =
    Term.map blk_t sub ~f:(live_def dead),
    Term.map blk_t ssa ~f:(fun b -> live_def dead b |> live_phi dead)

  let rec run subs =
    report_progress ~note:"ssa" ();
    let subs = Seq.memoize subs in
    report_progress ~note:"free-vars" ();
    let free = union ~init:Var.Set.empty ~f:free_vars subs in
    report_progress ~note:"dead-vars" ();
    let dead = union ~init:Tid.Set.empty ~f:(compute_dead free) subs in
    report_progress ~note:"clean" ();
    if Set.is_empty dead then subs
    else run (Seq.map subs ~f:(clean dead))

  let process prog =
    let ptid = Term.tid prog in
    let subs = Term.enum sub_t prog |> Seq.map ~f:(fun s ->
        let s = propagate_consts s in
        s, Sub.ssa s) in
    let subs = run subs in
    let len = Seq.length subs in
    let bld = Program.Builder.create ~tid:ptid ~subs:len () in
    Seq.iter ~f:(fun (s,_) -> Program.Builder.add_sub bld s) subs;
    Program.Builder.result bld
end

module Z = struct

  let free_vars (_,ssa) = free_vars ssa
  let compute_dead free (_,ssa) = compute_dead free ssa

  let clean is_live (sub,ssa) =
    let sub' =
      Term.map blk_t sub ~f:(Term.filter def_t ~f:is_live) |>
      propagate_consts in
    if Sub.equal sub sub' then (sub,ssa)
    else sub',Sub.ssa sub'

  let rec run subs =
    let subs = Seq.memoize subs in
    report_progress ~note:"free-vars" ();
    let free = union ~init:Var.Set.empty ~f:free_vars subs in
    report_progress ~note:"dead-vars" ();
    let dead = union ~init:Tid.Set.empty ~f:(compute_dead free) subs in
    let live t = not (Set.mem dead (Term.tid t)) in
    report_progress ~note:"clean" ();
    if Set.is_empty dead then subs
    else run (Seq.map subs ~f:(clean live))

  let process prog =
    let ptid = Term.tid prog in
    let subs =
      Term.enum sub_t prog |>  Seq.map ~f:(fun s -> s, Sub.ssa s) in
    let subs = run subs in
    let len = Seq.length subs in
    let bld = Program.Builder.create ~tid:ptid ~subs:len () in
    Seq.iter ~f:(fun (s,_) -> Program.Builder.add_sub bld s) subs;
    Program.Builder.result bld
end


let digest proj =
  let module Digest = Data.Cache.Digest in
  (object
    inherit [Digest.t] Term.visitor
    method! enter_arg t dst = Digest.add dst "%a" Arg.pp t
    method! enter_def t dst = Digest.add dst "%a" Def.pp t
    method! enter_jmp t dst = Digest.add dst "%a" Jmp.pp t
  end)#run
    (Project.program proj)
    (Digest.create ~namespace:"dead_code_elimination")


let run proj =
  let digest = digest proj in
  let p =
    match Program.Cache.load digest with
    | Some p -> p
    | None ->
      let p = Y.process (Project.program proj) in
      Program.Cache.save digest p;
      p in
  Project.with_program proj p

let () = Config.when_ready (fun _ ->
    Project.register_pass ~deps:["api"] ~autorun:true run)
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
