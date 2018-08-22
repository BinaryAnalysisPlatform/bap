open Core_kernel.Std
open Bap.Std
open Regular.Std
open Format
include Self()

module Diff = Dead_code_diff


type level =
  | Global
  | Virtual
  | Flags [@@deriving bin_io, sexp, compare]

let level_of_string s = level_of_sexp (Sexp.of_string s)
let level_to_string x = Sexp.to_string (sexp_of_level x)
let level_equal x y = compare_level x y = 0
let mem levels x = List.mem levels ~equal:level_equal x

let check_level is_flag levels var =
  mem levels Global ||
  (Var.is_virtual var && mem levels Virtual) ||
  (is_flag var && mem levels Flags)

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

let compute_dead checked protected sub =
  let defs,uses = computed_def_use sub in
  let dead = Set.diff defs uses in
  let live v = not (Set.mem dead v) in
  (object inherit [Tid.Set.t] Term.visitor
    method! enter_def t dead =
      let v = Def.lhs t in
      if Set.mem protected (Var.base v) || live v || not (checked v)
      then dead
      else Set.add dead (Term.tid t)
  end)#visit_sub sub Tid.Set.empty

let is_alive dead t = not (Set.mem dead (Term.tid t))
let live_phi dead blk = Term.filter phi_t ~f:(is_alive dead) blk

let live_def checked dead blk =
  Term.filter def_t blk ~f:(fun d ->
      if checked (Def.lhs d) then is_alive dead d
      else true)

let substitute sub vars =
  (object
    inherit Term.mapper as super
    method! map_var v =
      match Map.find vars v with
      | None -> Bil.var v
      | Some e -> e
    method! map_exp e = Exp.fold_consts (super#map_exp e)
  end)#map_sub sub

(* A simple constant propagation. Note, the input is required to be in SSA. *)
let propagate_consts checked sub =
  let vars = (object
    inherit [exp Var.Map.t] Term.visitor
    method! enter_def t vars =
      let v = Def.lhs t in
      if checked v then
        match Def.rhs t with
        | Bil.Unknown _ | Bil.Int _ as exp ->
          Map.add vars ~key:v ~data:exp
        | _ -> vars
      else vars
  end)#visit_sub sub Var.Map.empty in
  substitute sub vars

let clean checked dead sub =
  Term.map blk_t sub ~f:(fun b -> live_def checked dead b |> live_phi dead)

let return_from_ssa sub =
  let drop_index t = (object
    inherit Term.mapper
    method! map_sym var = Var.base var
  end)#map_sub t in
  Term.map blk_t sub ~f:(Term.filter phi_t ~f:(fun _ -> false)) |>
  drop_index

let is_flag arch =
  let module T = (val (target_of_arch arch)) in
  T.CPU.is_flag

let free_vars prog =
  report_progress ~note:"free-vars" ();
  Term.enum sub_t prog |>
  Seq.fold ~init:Var.Set.empty ~f:(fun free sub ->
      let free = Set.union free (Sub.free_vars sub) in
      Seq.fold ~init:free (Term.enum arg_t sub)
        ~f:(fun free arg ->
            Set.union free (Exp.free_vars (Arg.rhs arg))))

let union ~init ~f = List.fold ~init ~f:(fun xs x -> Set.union xs (f x))

let process arch prog levels =
  let checked = check_level (is_flag arch) levels in
  let free = free_vars prog in
  let rec run subs =
    report_progress ~note:"propagate consts" ();
    let subs = List.rev_map subs ~f:(propagate_consts checked) in
    report_progress ~note:"dead-vars" ();
    let dead = union ~init:Tid.Set.empty ~f:(compute_dead checked free) subs in
    report_progress ~note:"clean" ();
    if Set.is_empty dead then subs
    else run (List.rev_map subs ~f:(clean checked dead)) in
  let subs = Term.enum sub_t prog |> Seq.to_list_rev in
  report_progress ~note:"ssa form" ();
  let subs = List.rev_map ~f:Sub.ssa subs in
  let subs = run subs in
  let subs = List.rev_map ~f:return_from_ssa subs in
  report_progress ~note:"calculating diff" ();
  let diff = Diff.create prog subs in
  report_progress ~note:"apply diff" ();
  Diff.apply prog diff, diff

let digest proj levels =
  let module Digest = Data.Cache.Digest in
  let digest =
    (object
      inherit [Digest.t] Term.visitor
      method! enter_arg t dst = Digest.add dst "%a" Arg.pp t
      method! enter_def t dst = Digest.add dst "%a" Def.pp t
      method! enter_jmp t dst = Digest.add dst "%a" Jmp.pp t
    end)#run
      (Project.program proj)
      (Digest.create ~namespace:"dead_code_elimination") in
  List.fold ~init:digest levels
    ~f:(fun digest lvl -> Digest.add digest "%s" (level_to_string lvl))

let run levels proj =
  let digest = digest proj levels in
  let arch = Project.arch proj in
  let prog =
    match Diff.Cache.load digest with
    | Some diff ->
      Diff.apply (Project.program proj) diff
    | None ->
      let prog, res = process arch (Project.program proj) levels in
      Diff.Cache.save digest res;
      prog in
  Project.with_program proj prog

module Levels = struct
  let printer fmt t = Format.fprintf fmt "%s" (level_to_string t)

  let fail = sprintf
      "unknown value %s, possible values are: flags | global | virtual"

  let parser s =
    try `Ok (level_of_string s)
    with (Failure _) -> `Error (fail s)

  let t = Config.list (Config.converter parser printer Virtual)
end

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
      "A list of comma separated values that defines a level of optimization,
       i.e. what variables are considered in the analysis. Possible variants are:
       $(b,virtual), $(b,flags) for virual variables and flags, and
       $(b,global) for all variables. The default value is $(b, virtual,flags)" in
    Config.(param Levels.t ~default:[Virtual; Flags] ~doc "level") in

  Config.when_ready (fun {Config.get=(!)} ->
      Project.register_pass ~deps:["api"] ~autorun:true (run !level))
