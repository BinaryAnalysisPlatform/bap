open Core_kernel.Std
open Bap.Std
open Regular.Std
open Format
include Self()

let union ~init ~f =
  List.fold ~init ~f:(fun xs x -> Set.union xs (f x))

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

let free_vars sub =
  Seq.fold ~init:(Sub.free_vars sub) (Term.enum arg_t sub)
    ~f:(fun free arg ->
        Set.union free (Exp.free_vars (Arg.rhs arg)))

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

let is_alive dead t = not (Set.mem dead (Term.tid t))
let live_def dead blk = Term.filter def_t ~f:(is_alive dead) blk
let live_phi dead blk = Term.filter phi_t ~f:(is_alive dead) blk

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

   Note, the input is required to be in SSA.
*)

type sub_extended = {
  body : sub term;
  consts : exp Var.Map.t;
}

type subst = exp Var.Map.t [@@deriving bin_io, sexp, compare]

type data = {
  deads : Tid.Set.t;
  substs : subst Tid.Map.t
} [@@deriving bin_io, sexp, compare]


let of_sub s = { body = s; consts = Var.Map.empty }

let update_consts consts vars =
  Map.fold vars ~init:consts
    ~f:(fun ~key:v ~data:e consts -> Map.add consts (Var.base v) e)

let substitute sub vars =
  let subst exp =
    Map.fold vars ~init:exp ~f:(fun ~key:pat ~data:rep ->
        Exp.substitute Bil.(var pat) rep) in
  Term.map blk_t sub ~f:(Blk.map_exp ~f:(fun exp ->
      subst exp |> Exp.fold_consts))

let propagate_consts use_real sub =
  let vars = (object inherit [exp Var.Map.t] Term.visitor

    method! enter_phi t vars =
      let lhs = Var.base (Phi.lhs t) in
      Map.filteri vars ~f:(fun ~key:v ~data:_ ->
          not (Var.equal (Var.base v) lhs))

    method! enter_def t vars =
      let v = Def.lhs t in
      if Var.is_virtual v || use_real
      then
        match Def.rhs t with
        | Bil.Unknown _ | Bil.Int _ as exp ->
          Map.add vars ~key:v ~data:exp
        | _ -> vars
      else vars
  end)#visit_sub sub.body Var.Map.empty in
  { body = substitute sub.body vars;
    consts = update_consts sub.consts vars; }

let clean_sub sub dead =
  Term.map blk_t sub ~f:(fun b -> live_def dead b |> live_phi dead)

let clean dead sub = { sub with body = clean_sub sub.body dead }
let free_vars sub = free_vars sub.body
let compute_dead free sub = compute_dead free sub.body

let remove_deads deads prog =
  Term.map sub_t prog
    ~f:(Term.map blk_t
          ~f:(Term.filter def_t ~f:(is_alive deads)))

let collect_consts subs =
  List.fold subs ~init:Tid.Map.empty
    ~f:(fun all s -> Map.add all (Term.tid s.body) s.consts)

let update prog {deads; substs } =
  report_progress ~note:"apply changes" ();
  Term.map sub_t prog
    ~f:(fun sub ->
        match Map.find substs (Term.tid sub) with
        | None -> sub
        | Some vars -> substitute sub vars) |>
  remove_deads deads

let process prog use_real =
  let rec run dead subs =
    report_progress ~note:"propagate consts" ();
    let subs = List.rev_map subs ~f:(propagate_consts use_real) in
    report_progress ~note:"free-vars" ();
    let free = union ~init:Var.Set.empty ~f:free_vars subs in
    report_progress ~note:"dead-vars" ();
    let dead' = union ~init:dead ~f:(compute_dead free) subs in
    report_progress ~note:"clean" ();
    if Set.length dead' > Set.length dead then
      run dead' (List.rev_map subs ~f:(clean dead'))
    else dead', subs in
  let subs = Term.enum sub_t prog |> Seq.map ~f:of_sub |> Seq.to_list_rev in
  report_progress ~note:"ssa form" ();
  let subs = List.rev_map ~f:(fun s -> {s with body = Sub.ssa s.body}) subs in
  let deads, subs = run Tid.Set.empty subs in
  let substs = collect_consts subs in
  let data = { deads; substs; } in
  let prog = update prog data in
  prog, data


module Dead_code_data = struct
  include Regular.Make(struct
      type nonrec t = data [@@deriving bin_io, sexp, compare]
      let version = version
      let module_name = None
      let hash = Hashtbl.hash
      let pp fmt t =
        Format.fprintf fmt "dead tids: ";
        Set.iter t.deads ~f:(Format.fprintf fmt "%a " Tid.pp);
        Format.pp_print_newline fmt ();
        Format.fprintf fmt "substitutions: ";
        Map.iteri t.substs ~f:(fun ~key:tid ~data:subst ->
            if not (Map.is_empty subst) then
              let () = Format.fprintf fmt "%s: " (Tid.name tid) in
              let () = Map.iteri subst ~f:(fun ~key:v ~data:e ->
                  Format.fprintf fmt "(%a %a) " Var.pp v Exp.pp e) in
              Format.pp_print_newline fmt ());
    end)
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

let run use_real proj =
  let digest = digest proj in
  let prog =
    match Dead_code_data.Cache.load digest with
    | Some x -> update (Project.program proj) x
    | None ->
      let prog, res = process (Project.program proj) use_real in
      Dead_code_data.Cache.save digest res;
      prog in
  Project.with_program proj prog

let () =
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
  ];
  let use_real =
    let doc = "Propagates consts in registers" in
    Config.flag ~doc "use-real" in

  Config.when_ready (fun {Config.get} ->
      Project.register_pass ~deps:["api"] ~autorun:true (run (get use_real)))
