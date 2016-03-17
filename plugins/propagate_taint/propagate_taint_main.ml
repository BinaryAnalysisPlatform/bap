open Core_kernel.Std
open Graphlib.Std
open Bap.Std
open Microx.Std
open Format
include Self()

type 'a scheme = {
  foreground : 'a option;
  background : 'a option;
}

type policy = Concretizer.policy


type args = {
  max_trace : int;
  max_loop  : int;
  deterministic : bool;
  policy : policy;
  interesting : string list;
  seeded  : color scheme;
  visited : color scheme;
  tainted : color scheme;
  tainted_regs : color scheme;
  tainted_ptrs : color scheme;
} [@@deriving fields]

class bir_mapper = object(self)
  method run p = Term.map sub_t ~f:(fun t -> self#map_sub t) p
  method map_term : 't 'p . ('p,'t) cls ->  't term -> 't term = fun _ t -> t
  method map_sub sub = self#map_term sub_t sub |>
                       Term.map arg_t ~f:(fun t -> self#map_arg t) |>
                       Term.map blk_t ~f:(fun t -> self#map_blk t)
  method map_blk blk = self#map_term blk_t blk |>
                       Term.map phi_t ~f:(fun t -> self#map_phi t) |>
                       Term.map def_t ~f:(fun t -> self#map_def t) |>
                       Term.map jmp_t ~f:(fun t -> self#map_jmp t)
  method map_arg : arg term -> arg term = self#map_term arg_t
  method map_phi : phi term -> phi term = self#map_term phi_t
  method map_def : def term -> def term = self#map_term def_t
  method map_jmp : jmp term -> jmp term = self#map_term jmp_t
end

type mapper = {map : 'a. 'a term -> 'a term}


let is_seeded t =
  Term.has_attr t Taint.reg ||
  Term.has_attr t Taint.ptr

class marker args ctxt = object(self)
  inherit bir_mapper as super
  method! map_term : 't 'p . ('p,'t) cls -> 't term -> 't term = fun _ t ->
    let is_visited  = Map.mem ctxt#visited (Term.tid t) in
    let is_seeded =
      Term.has_attr t Taint.reg || Term.has_attr t Taint.ptr in
    let has_tainted taints = not (Map.is_empty (taints (Term.tid t))) in
    let regs = ctxt#tainted_regs in
    let ptrs = ctxt#tainted_ptrs in
    let is_tainted = has_tainted regs || has_tainted ptrs in
    self#mark args.visited is_visited t |>
    self#mark args.seeded  is_seeded |>
    self#mark args.tainted is_tainted |>
    self#mark args.tainted_regs (has_tainted regs) |>
    self#mark args.tainted_ptrs (has_tainted ptrs) |>
    self#taint Taint.regs has_tainted regs |>
    self#taint Taint.ptrs has_tainted ptrs

  method private taint : 't . 'a -> 'b -> 'c -> 't term -> 't term =
    fun taint tainted taints t ->
      if tainted taints
      then Term.set_attr t taint (taints (Term.tid t))
      else t

  method private mark : 't . color scheme -> bool -> 't term -> 't term =
    fun scheme marked t ->
      let map scheme attr t =
        if marked
        then Option.value_map scheme ~default:t
            ~f:(fun color -> Term.set_attr t attr color)
        else t in
      map scheme.foreground foreground t |>
      map scheme.background background
end

let contains_seed sub =
  let has t p = Term.enum t p |> Seq.exists ~f:is_seeded in
  has arg_t sub || Term.enum blk_t sub |> Seq.exists ~f:(fun blk ->
      has phi_t blk || has def_t blk)

let seeded callgraph subs =
  let callers sub =
    Graphlib.fold_reachable (module Graphs.Callgraph) callgraph
      ~rev:true ~init:Tid.Set.empty ~f:Set.add (Term.tid sub) in
  Seq.filter subs ~f:contains_seed |>
  Seq.fold ~init:Tid.Set.empty ~f:(fun subs sub ->
      Set.union subs @@ callers sub)

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

type stats = {
  sub_count : int;
  sub_total : int;
  visited : Tid.Set.t;
  terms : Tid.Set.t;
}

let stats sub_total = {
  sub_count = 0;
  sub_total;
  visited = Tid.Set.empty;
  terms = Tid.Set.empty;
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

let keys = Map.fold ~init:Tid.Set.empty ~f:(fun ~key ~data set ->
    Set.add set key)

let visited_sub sub stat res = {
  stat with
  visited = Set.union stat.visited @@ keys res#visited;
}

let main args proj =
  let prog = Project.program proj in
  let callgraph = Program.to_graph prog in
  let is_interesting = match args.interesting with
    | [] -> fun _ -> true
    | xs -> fun sub -> List.mem xs (Sub.name sub) in
  let subs = Term.enum sub_t prog |>
             Seq.filter ~f:is_interesting |>
             seeded callgraph in
  let proj,stat =
    Term.enum sub_t prog |>
    Seq.filter ~f:(fun sub -> Set.mem subs (Term.tid sub)) |>
    Seq.fold ~init:(proj,stats (Set.length subs))
      ~f:(fun (proj,stat) sub ->
          let stat = entered_sub stat sub in
          eprintf "%-40s %a\r%!" (Sub.name sub) pp_progressbar stat;
          let ctxt = Propagator.run
              ~max_steps:args.max_trace
              ~max_loop:args.max_loop
              ~deterministic:args.deterministic
              ~policy:args.policy
              proj (`Term (Term.tid sub)) in
          let marker = new marker args ctxt in
          let prog = Project.program proj |> marker#run in
          let stat = visited_sub sub stat ctxt in
          Project.with_program proj prog, stat) in
  printf "@.Coverage: %a@." pp_coverage stat;
  proj

module Cmdline = struct

  open Cmdliner

  let max_trace : int Term.t =
    let doc = "Limit maximum trace length to $(docv)" in
    Arg.(value & opt int 1_000_000 &
         info ["max-trace"] ~doc ~docv:"BLOCKS")

  let max_loop : int Term.t =
    let doc = "Limit loop to $(docv) iterations" in
    Arg.(value & opt int 10 &
         info ["max-iterations"] ~doc ~docv:"N")

  let colors = [
    "black",   `black;
    "red",     `red;
    "green",   `green;
    "yellow",  `yellow;
    "blue",    `blue;
    "magenta", `magenta;
    "cyan",    `cyan;
    "white",   `white;
  ]

  let mark name side : color option Term.t =
    let doc = sprintf "Mark %s terms with the given color.
    Accepted values are %s" name (Arg.doc_alts_enum colors) in
    Arg.(value & opt (some (enum colors)) None &
         info [sprintf "mark-%s-%s" name side]  ~doc)

  let mark_visited_foreground = mark "visited" "foreground"
  let mark_visited_background = mark "visited" "background"
  let mark_seeded_foreground = mark "seeded" "foreground"
  let mark_seeded_background = mark "seeded" "background"
  let mark_tainted_foreground = mark "tainted" "foreground"
  let mark_tainted_background = mark "tainted" "background"
  let mark_tainted_regs_foreground = mark "tainted-regs" "foreground"
  let mark_tainted_regs_background = mark "tainted-regs" "background"
  let mark_tainted_ptrs_foreground = mark "tainted-ptrs" "foreground"
  let mark_tainted_ptrs_background = mark "tainted-ptrs" "background"

  let interesting : string list Term.t =
    let doc = "Look only at specified functions" in
    Arg.(value & opt (list string) [] & info ["interesting"] ~doc)

  let deterministic : bool Term.t =
    let doc = "Run in a deterministic mode. In this mode we will
              follow only one execution path, without backtracking,
              giving a more feasable result, but much less coverage" in
    Arg.(value & flag & info ["deterministic"] ~doc)

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

    let t : t Arg.converter = parser,printer
  end

  let policy : policy Term.t =
    let doc = "Input generation policy. If set to a fixed value,
      e.g. `0', then all unknown data will be concretized to this
      value. If set to an interval, e.g., `(0 5)', then values will
      be randomly picked from this interval (boundaries including).
      If set to `random', then values will be picked randomly from a
      domain, defined by a type of value." in
    Arg.(value & opt Policy.t (`Fixed 0L) & info ["policy"] ~doc)


  let create max_trace max_loop deterministic policy interesting
      seedf seedb visitedf visitedb  taintedf taintedb
      taintedrf taintedrb  taintedpf taintedpb = {
    max_trace; max_loop; deterministic; policy; interesting;
    seeded  = {foreground=seedf;    background=seedb};
    visited = {foreground=visitedf; background=visitedb};
    tainted = {foreground=taintedf; background=taintedb};
    tainted_regs = {foreground=taintedrf; background=taintedrb};
    tainted_ptrs = {foreground=taintedpf; background=taintedpb};
  }

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
    the result. This pass itself doesn't provide any analysis, other
    than the ability to highlight terms with specific colors.";


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

    `S "EXAMPLE";
    `Pre " bap exe --saluki-seed --propagate-taint --saluki-solve "

  ]

  let grammar =
    Term.(pure create $max_trace $max_loop $deterministic
          $policy $interesting
          $mark_seeded_foreground $mark_seeded_background
          $mark_visited_foreground $mark_visited_background
          $mark_tainted_foreground $mark_tainted_background
          $mark_tainted_regs_foreground $mark_tainted_regs_background
          $mark_tainted_ptrs_foreground $mark_tainted_ptrs_background)

  let info = Term.info ~man ~doc name

  let args argv = match Term.eval ~argv (grammar,info) with
    | `Ok args -> args
    | `Version | `Help -> exit 0
    | `Error _ -> exit 1
end

let () =
  let args = Cmdline.args argv in
  Project.register_pass (main args)
