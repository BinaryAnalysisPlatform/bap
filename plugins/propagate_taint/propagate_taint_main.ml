open Core_kernel.Std
open Graphlib.Std
open Bap.Std
open Microx.Std
open Format
include Self()


type policy = Concretizer.policy

let is_seeded t =
  Term.has_attr t Taint.reg ||
  Term.has_attr t Taint.ptr

let is_visited ctxt t  = Map.mem ctxt#visited (Term.tid t)

let has_seed _ctxt t = is_seeded t

let has_tainted what ctxt t =
  not (Map.is_empty ((what ctxt) (Term.tid t)))

let has_tainted_regs t = has_tainted (fun c -> c#tainted_regs) t
let has_tainted_ptrs t = has_tainted (fun c -> c#tainted_ptrs) t
let is_tainted c t = has_tainted_regs c t || has_tainted_ptrs c t


module Scheme = struct
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

  let expected assoc =
    List.map ~f:fst assoc |> List.map ~f:(sprintf "%S") |>
    String.concat ~sep:" | "

  let expect got assoc =
    `Error (sprintf "got %S expected %s" got @@ expected assoc)

  let color_t s = match List.Assoc.find colors s with
    | Some c -> `Ok c
    | None -> expect s colors

  let string s = `Ok s
  let unit _ = `Ok ()
  let float s =
    try `Ok (Float.of_string s) with exn ->
      `Error (sprintf "got %s expected <float>" s)
  let tid s = try `Ok (Tid.from_string_exn s) with
    | Invalid_argument s -> `Error s
    | exn -> `Error (Exn.to_string exn)

  type tagger = {tag : 'a. 'a term -> 'a term}
  type 'a result = [`Ok of 'a | `Error of string]

  type tag_parser = string -> string ->
    [`Ok of tagger | `Error of string] option

  let tag tag parse : string * tag_parser =
    Value.Tag.name tag, fun name input ->
      if Value.Tag.name tag <> name then None
      else Option.some @@ match parse input with
        | `Ok v -> `Ok {tag = fun t -> Term.set_attr t tag v}
        | `Error e -> `Error e

  let tags : (string * tag_parser) list = [
    tag foreground color_t;
    tag background color_t;
    tag color color_t;
    tag comment string;
    tag python string;
    tag mark unit;
    tag weight float;
    tag Taint.reg tid;
    tag Taint.ptr tid;
  ]

  let parse_tag (x : string) y = List.find_map tags ~f:(fun (_,p) -> p x y) |> function
    | None -> expect x tags
    | Some thing -> thing

  let conjunct ~f m1 m2 : 'a result = match m1,m2 with
    | `Ok m1, `Ok m2 -> `Ok (f m1 m2)
    | `Error e,_ | _,`Error e -> `Error e

  let conjunct_marks =
    conjunct ~f:(fun m1 m2 -> {tag = fun t -> m2.tag (m1.tag t)})

  let rec parse_marks = function
    | Sexp.List [Sexp.Atom tag] | Sexp.Atom tag -> parse_tag tag ""
    | Sexp.List [Sexp.Atom tag; Sexp.Atom v] -> parse_tag tag v
    | Sexp.List marks ->
      List.map marks ~f:parse_marks |>
      List.fold ~init:(`Ok {tag = ident}) ~f:conjunct_marks

  type pred = { matches : 'a. Propagator.result -> 'a term -> bool}

  let preds : (string * pred) list = [
    "visited",          {matches = is_visited};
    "has-seed",         {matches = has_seed};
    "has-tainted-regs", {matches = has_tainted_regs};
    "has-tainted-ptrs", {matches = has_tainted_ptrs};
    "has-taint",        {matches = is_tainted};
  ]

  let conjunct_preds = conjunct ~f:(fun p1 p2 -> {
        matches = fun c t -> p1.matches c t && p2.matches c t
      })

  let parse_pred s = match List.Assoc.find preds s with
    | Some thing -> `Ok thing
    | None -> expect s preds

  let rec parse_preds = function
    | Sexp.Atom p -> parse_pred p
    | Sexp.List [] -> `Error "Expected non-empty set of predicates"
    | Sexp.List ps -> List.map ps ~f:parse_preds
                      |> List.reduce_exn ~f:conjunct_preds

  type marker = { mark : 'a. Propagator.result -> 'a term -> 'a term }
  type t = marker
  let default = {mark = fun _ t -> t}

  let marker pred tagger = {
    mark = fun ctxt t ->
      if pred.matches ctxt t then tagger.tag t else t
  }

  let join m1 m2 = {mark = fun ctxt t -> m2.mark ctxt (m1.mark ctxt t)}

  let conjunct_markers = conjunct ~f:join

  let merge ms = List.fold ~init:default ~f:join ms

  let parse_marker ps ms = match parse_preds ps, parse_marks ms with
    | `Ok ps, `Ok ms -> `Ok (marker ps ms)
    | `Error e,_|_,`Error e -> `Error e

  let parse = function
    | Sexp.List [preds; marks] -> parse_marker preds marks
    | _ -> `Error {|expect "("<preds> <marks>")"|}

  let sexp_error {Sexp.location; err_msg} =
    `Error (sprintf "Syntax error: %s - %s" location err_msg)

  let parse_string s =
    try parse (Sexp.of_string s)
    with Sexp.Parse_error err -> sexp_error err
       | exn -> `Error "Malformed sexp"

  let parse_file f =
    try List.map ~f:parse (Sexp.load_sexps f) |>
        List.fold ~init:(`Ok default) ~f:conjunct_markers
    with Sexp.Parse_error err -> sexp_error err
       | Sys_error e -> `Error e
       | exn -> `Error "Malformed sexp "

  let print ppf t = ()

  let t = parse_string, print
end


type args = {
  max_trace : int;
  max_loop  : int;
  deterministic : bool;
  policy : policy;
  interesting : string list;
  marker : Scheme.marker
} [@@deriving fields]

class marker m ctxt = object(self)
  inherit Term.mapper as super
  method! map_term cls t =
    let t = super#map_term cls t in
    let has_tainted taints = not (Map.is_empty (taints (Term.tid t))) in
    let taint taint tainted taints t =
      if tainted taints
      then Term.set_attr t taint (taints (Term.tid t))
      else t in
    let regs = ctxt#tainted_regs in
    let ptrs = ctxt#tainted_ptrs in
    m.Scheme.mark ctxt t  |>
    taint Taint.regs has_tainted regs |>
    taint Taint.ptrs has_tainted ptrs
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
          let marker = new marker args.marker ctxt in
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

  let interesting : string list Term.t =
    let doc = "Look only at specified functions" in
    Arg.(value & opt (list string) [] & info ["interesting"] ~doc)

  let deterministic : bool Term.t =
    let doc = "Run in a deterministic mode. In this mode we will
              follow only one execution path, without backtracking,
              giving a more feasable result, but much less coverage" in
    Arg.(value & flag & info ["deterministic"] ~doc)

  let scheme : Scheme.t list Term.t =
    let doc = "Mark terms according the scheme $(docv)" in
    Arg.(value & opt_all Scheme.t [] &
         info ["mark-scheme"] ~doc ~docv:"SCHEME")

  let scheme_file : string option Term.t =
    let doc = "File with color scheme" in
    Arg.(value & opt (some file) None & info ["mark-scheme-from-file"] ~doc)

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


  let create
      max_trace max_loop deterministic policy interesting markers scm = {
    max_trace; max_loop; deterministic; policy; interesting;
    marker = match scm with
      | None -> Scheme.merge markers
      | Some file -> match Scheme.parse_file file with
        | `Ok m -> Scheme.merge (m :: markers)
        | `Error e -> invalid_arg e
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

    `P "Although the pass itself doesn't perform any analysis it can
    mark terms with attributes. Terms are marked according to a
    marking scheme specified with $(b,--mark-scheme)=$(i,SCHEME) or
    $(i,--mark-scheme-file)=$(i,FILE). Each entry of the $(i,FILE), or
    $(i,SCHEME) argument must conform to the following grammar:";

    `Pre begin sprintf "
      SCM    ::= (PREDS MARKS)
      PREDS  ::= PRED | (PRED1 .. PREDn)
      MARKS  ::= MARK | (MARK1 .. MARKn)
      MARK   ::= TAG  | (TAG VALUE)
      PRED   ::= %s
      TAG    ::= %s
    " Scheme.(expected preds) Scheme.(expected tags)
    end;

    `P "Each $(i,SCHEME) is a pair consisting of a set of predicates
    and a set of marks. If all predicates matches, then all marks are
    applied to a term. Mark is represented by a pair consisting of tag
    name and a value. If tag value is of type unit, then it is just a
    tag.";

    `S "EXAMPLE";
    `Pre " bap exe --saluki-seed --propagate-taint --saluki-solve ";
    `Pre {| bap exe --mark-addr=0xBADADR --propagate-taint
            --propagate-taint-mark-scheme=
            '((visited has-taint) ((comment "gotcha") (foreground red)))' |};
  ]

  let grammar =
    Term.(pure create $max_trace $max_loop $deterministic
          $policy $interesting $scheme $scheme_file)

  let info = Term.info ~man ~doc name

  let args argv = match Term.eval ~argv (grammar,info) with
    | `Ok args -> args
    | `Version | `Help -> exit 0
    | `Error _ -> exit 1
end

let () =
  let args = Cmdline.args argv in
  Project.register_pass (main args)
