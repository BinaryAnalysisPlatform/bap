open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std
open Microx.Std
open Format
include Self()

type policy = Concretizer.policy  [@@deriving sexp_of]

type args = {
  max_trace : int;
  max_loop  : int;
  deterministic : bool;
  random_seed : int option;
  reg_policy : policy;
  mem_policy : policy;
  interesting : string list;
} [@@deriving fields, sexp_of]

let map_if cond t ~f = if cond then f t else t

class marker res = object(self)
  inherit Term.mapper as super
  method! map_term cls t =
    let module State = Propagator.Result in
    let t = super#map_term cls t in
    let tid = Term.tid t in
    let taint taint taints =
      let taints = taints tid in
      map_if (not (Map.is_empty taints)) ~f:(fun t ->
          Term.set_attr t taint taints) in
    let regs = State.tainted_regs res in
    let ptrs = State.tainted_ptrs res in
    map_if (State.is_visited res tid) t ~f:(fun t ->
        Term.set_attr t Term.visited ()) |>
    taint Taint.regs regs |>
    taint Taint.ptrs ptrs
end

let is_seeded t =
  Term.has_attr t Taint.reg ||
  Term.has_attr t Taint.ptr

let contains_seed sub =
  Term.enum blk_t sub |> Seq.exists ~f:(fun blk ->
      Term.enum def_t blk |> Seq.exists ~f:is_seeded)

let seeded callgraph subs =
  let callers sub =
    Graphlib.fold_reachable (module Graphs.Callgraph) callgraph
      ~rev:true ~init:Tid.Set.empty ~f:Set.add (Term.tid sub) in
  Seq.filter subs ~f:contains_seed |>
  Seq.fold ~init:Tid.Set.empty ~f:(fun subs sub ->
      Set.add (Set.union subs @@ callers sub) (Term.tid sub))

let tids_of_sub sub =
  let terms t p =
    Term.enum t p |> Seq.fold ~init:Tid.Set.empty ~f:(fun set t ->
        Set.add set (Term.tid t)) in
  let (++) = Set.union in
  let init = terms arg_t sub in
  Term.enum blk_t sub |> Seq.fold ~init ~f:(fun sum blk ->
      terms phi_t blk ++
      terms def_t blk ++
      terms jmp_t blk ++ sum)

module State = struct
  module Taints = Propagator.Result
  type t = {
    sub_count : int;
    sub_total : int;
    visited : Tid.Set.t;
    terms : Tid.Set.t;
    taints : Taints.t;
  } [@@deriving bin_io, compare, sexp]

  let create sub_total = {
    sub_count = 0;
    sub_total;
    visited = Tid.Set.empty;
    terms = Tid.Set.empty;
    taints = Taints.empty;
  }

  let keys = Map.fold ~init:Tid.Set.empty ~f:(fun ~key ~data set ->
      Set.add set key)

  let update_taints t taints = {
    t with
    taints = Taints.union t.taints taints;
    visited = Set.union t.visited @@
      keys (Propagator.Result.visited taints);
  }

  let percent (x,y) =
    if y = 0 then 0
    else Int.of_float (100. *. (float x /. float y))

  let pp_ratio ppf (x,y) =
    fprintf ppf "[%d/%d] %3d%%" x y (percent (x,y))

  let pp_progressbar ppf {sub_count=x; sub_total=y} =
    fprintf ppf "%a" pp_ratio (x,y)

  let coverage visited terms =
    let x = Set.length (Set.inter terms visited) in
    let y = Set.length terms in
    x,y

  let pp_coverage ppf {visited; terms} =
    pp_ratio ppf (coverage visited terms)

  let entered_sub stat sub = {
    stat with
    sub_count = stat.sub_count + 1;
    terms = Set.union stat.terms (tids_of_sub sub)
  }

  let taints t = t.taints

  include Regular.Make(struct
      type nonrec t = t [@@deriving bin_io, compare, sexp]
      let version = version
      let module_name = None
      let hash = Hashtbl.hash
      let pp = pp_progressbar
    end)
end

let digest_project proj =
  let module Digest = Data.Cache.Digest in
  let add_taint tag t dst = match Term.get_attr t tag with
    | None -> dst
    | Some v -> Digest.add dst "%a" Tid.pp v in
  (object
    inherit [Digest.t] Term.visitor
    method enter_term cls t dst =
      add_taint Taint.reg t dst |>
      add_taint Taint.ptr t
    method enter_arg t dst = Digest.add dst "%a" Arg.pp t
    method enter_def t dst = Digest.add dst "%a" Def.pp t
    method enter_jmp t dst = Digest.add dst "%a" Jmp.pp t
  end)#run
    (Project.program proj)
    (Data.Cache.Digest.create ~namespace:"propagate_taint")

let digest_args args =
  sexp_of_args args |> Sexp.to_string_mach |> Digest.string

let digest args proj =
  Data.Cache.Digest.(add_sexp (digest_project proj)
                       sexp_of_args args)

let process args proj =
  let prog = Project.program proj in
  let callgraph = Program.to_graph prog in
  let is_interesting = match args.interesting with
    | [] -> fun _ -> true
    | xs -> fun sub -> List.mem xs (Sub.name sub) in
  let subs = Term.enum sub_t prog |>
             Seq.filter ~f:is_interesting |>
             seeded callgraph in
  Term.enum sub_t prog |>
  Seq.filter ~f:(fun sub -> Set.mem subs (Term.tid sub)) |>
  Seq.fold ~init:(State.create (Set.length subs)) ~f:(fun s sub ->
      let s = State.entered_sub s sub in
      eprintf "%-40s %a\r%!" (Sub.name sub) State.pp_progressbar s;
      Propagator.run
        ~max_steps:args.max_trace
        ~max_loop:args.max_loop
        ~deterministic:args.deterministic
        ?random_seed:args.random_seed
        ~reg_policy:args.reg_policy
        ~mem_policy:args.mem_policy
        proj (`Term (Term.tid sub)) |>
      State.update_taints s)

let main args proj =
  let digest = digest args proj in
  let state = match State.Cache.load digest with
    | Some s -> s
    | None ->
      let s = process args proj in
      State.Cache.save digest s;
      s in
  eprintf "@.Coverage: %a@." State.pp_coverage state;

  let marker = new marker (State.taints state) in
  Project.program proj |> marker#run |>
  Project.with_program proj

module Cmdline = struct

  let man = [
    `S "DESCRIPTION";

    `P "A taint propagation framework, that uses microexecution to
    propagate the taint through a program. The execution is perfomed
    using the ConquEror Engine, that is short for Concrete Evaluation
    with Errors. This execution engine allows to run incomplete
    programs with an unspecified user input. Moreover, to increase the
    coverage it may take infeasible paths.";


    `P "The taint is propagated from a seed to its maximum extent. The
    seed is a definition point that is marked with a `Taint.reg` or
    `Taint.ptr` tag. A usual way of using the framework, would be to
    use one or more passes that marks points of interest with a taint
    seed, then to use the `propagate-taint` pass to propagate the
    taint, and, finally, to use a pass that will collect and analyze
    the result.";


    `P "The microexecution is performed over a lifted program using
    bap-microx library. Memory reads are intercepted and redirected to
    program image, if possible (for static data), otherwise they are
    concretized. All other inputs, like reads from unknown registers
    or user input are also concretized. Several concretization
    policies are provided:";

    `Pre "
      - Const - all unknown values are concretized to a specified constant;
      - Random - a random value is picked from a value domain;
      - Range - a random value is picked from a specified range.";

    `P "By default, the microexecution engine tries to visit all
    program branches. During the execution, it will record missed
    branches as checkpoints. When there is nothing more to explore, it
    will backtrack to a stored checkpoint, restoring the execution
    state at this program point, and continue the execution. Of
    course, in this case the state will contradict with a path
    constraint. In a deterministic mode the bactracking mechanism is
    disabled. In this mode, no checkpoints are recorded, and whenever
    the interpreter requests a backtracking, it will instead return
    from a current procedure.";

    `P "The maximum length of an execution path is limited with some
    constant number of jumps (basic blocks). Also, a loop escaping
    mechanism, will detect loops and bail out of them after a
    specified amount of iterations. In the deterministic mode it will
    just return from a procedure, otherwise, it will backtrack.";

  ]

  let max_trace = Config.(param int "max-trace"
                            ~default:1_000_000 ~docv:"BLOCKS"
                            ~doc:"Limit maximum trace length to $(docv)")

  let max_loop = Config.(param int "max-iterations"
                           ~default:10 ~docv:"N"
                           ~doc:"Limit loop to $(docv) iterations")

  let interesting = Config.(param (list string) "interesting"
                              ~doc:"Look only at specified functions")

  let deterministic = Config.(flag "deterministic"
                                ~doc:"Run in a deterministic mode. \
                                      In this mode we will follow only \
                                      one execution path, without \
                                      backtracking, giving a more \
                                      feasable result, but much less \
                                      coverage")
  module Policy = struct
    type t = policy


    let num = Int64.of_string
    let error = `Error "policy ::= random | <num> | (<num> <num>)"

    let fixed n = try `Ok (`Fixed (num n)) with exn -> error
    let interval b e =
      try `Ok (`Interval (num b, num e)) with exn -> error

    let parser input = match Sexp.of_string input with
      | Sexp.Atom "random" -> `Ok `Random
      | Sexp.Atom n -> fixed n
      | Sexp.List [Sexp.Atom b; Sexp.Atom e] -> interval b e
      | _ -> error

    let printer ppf = function
      | `Random -> fprintf ppf "random"
      | `Fixed n -> fprintf ppf "%Ld" n
      | `Interval (n,m) -> fprintf ppf "(%Ld %Ld)" n m

    let t default : t Config.converter =
      Config.converter parser printer default
  end

  let policy key name default : policy Config.param =
    let doc = sprintf "Input generation policy. If set to a fixed value,
      e.g. `0', then all undefined %s will be concretized to this
      value. If set to an interval, e.g., `(0 5)', then values will
      be randomly picked from this interval (boundaries including).
      If set to `random', then values will be picked randomly from a
      domain, defined by a type of value." name in
    Config.(param (Policy.t default) (sprintf "%s-value" key) ~doc)

  let random_seed : int option Config.param =
    let doc =
      "Initialize random number generator with the given seed" in
    Config.(param (some int) "random-seed" ~doc)

  let create
      max_trace max_loop deterministic
      random_seed reg_policy mem_policy
      interesting = {
    max_trace; max_loop; deterministic;
    random_seed; reg_policy; mem_policy; interesting;
  }

  let () =
    let reg = policy "reg" "registers" `Random in
    let mem = policy "mem" "memory locations" `Random in
    Config.when_ready (fun {Config.get=(!)} ->
        let args = create !max_trace !max_loop !deterministic
            !random_seed !reg !mem !interesting in
        Project.register_pass (main args))

end
