open Core_kernel
open Format

(* Algorithm description.

   A fact working state is represented as a list of hypotheses, where
   each hypothesis is represented as a list of patterns that should be
   matched, and a mapping B from variables to tuples.

   We start with the rule left hand side and an empty mapping B as
   our initial hypothesis. Every time a new fact arrives we apply it
   to all hypothesis. If a hypothesis matches the fact, then a new
   hypothesis is created that has one less pattern, and possibly
   increased mapping B. The original hypothesis is preserved. All
   hypotheses that has an empty list of patterns are removed at the
   final phase, and their corresponding actions are fired with
   variables substituted using the final binding.

   The algorithm maintains the match state and shares (via the OCaml
   heap sharing) the state between different valuations of the same
   rule. At this stage we decided not to share the state between
   different rules, neither do we have the global set of rules,
   allowing a user to operate on a per-rule basis.*)

type tuple = Sexp.t
type fact = tuple

module Rule = struct


  type rule = {
    patterns : tuple list;
    template : fact list;
    bindings : tuple String.Map.t;
  }

  type hypot = {
    rule : rule;
    work : rule list;
  }


  type t = hypot

  type range = Parsexp.Positions.range
  type pos = Parsexp.Positions.pos
  type positions = Parsexp.Positions.t
  type sexp_error = Parsexp.Parse_error.t

  type hint =
    | Expect_list of range
    | Expect_pair

  type error =
    | Unbound of string * range
    | Wildcard of range
    | Parse_error of hint * range
    | Not_a_sexp of sexp_error

  let lhs {rule} = rule.patterns
  let rhs {rule} = rule.template
  let sexp_of_t {rule} = Sexp.List [
      List rule.patterns;
      List rule.template;
    ]

  let reset {rule} = {rule; work = [rule]}

  let empty = String.Map.empty
  let binding x rhs = String.Map.singleton x rhs

  let rec sexp_fold_atoms ~get_pos ~init ~f sexp =
    let rec loop init s = match s with
      | Sexp.Atom x -> f init (get_pos s) x
      | Sexp.List xs -> List.fold ~init ~f:loop xs in
    loop init sexp

  let is_variable x = String.length x > 0 && x.[0] = '?'

  let vars_of_sexp ~get_pos init xs =
    sexp_fold_atoms ~get_pos ~init xs ~f:(fun atoms loc atom ->
          if is_variable atom
          then Map.add atoms ~key:atom ~data:loc
          else atoms)

  let collect_free_vars ~get_pos xs =
    List.fold ~init:String.Map.empty ~f:(vars_of_sexp ~get_pos) xs

  let check_rule ~get_pos lhs rhs =
    let def = collect_free_vars ~get_pos lhs
    and use = collect_free_vars ~get_pos rhs in
    match Map.find use "?" with
    | Some pos ->  Error (Wildcard pos)
    | None -> Map.merge def use ~f:(fun ~key -> function
        | `Both _ -> None
        | `Left _ -> None
        | `Right p -> Some p) |>
              Map.min_elt |> function
              | Some (var,pos) -> Error (Unbound (var,pos))
              | None -> Ok ()

  let of_sexp ~get_pos sexp =
    match (sexp : Sexp.t) with
    | Atom _ as bad
    | List [Atom _ as bad; _]
    | List [_; Atom _ as bad] ->
      Error (Parse_error ((Expect_list (get_pos bad)), get_pos sexp))
    | List [] | List [_] | List (_ :: _ :: _ :: _) ->
      Error (Parse_error (Expect_pair, get_pos sexp))
    | List [
        List patterns;
        List template;
      ] -> match check_rule ~get_pos patterns template with
      | Ok () ->
        let rule = {patterns; template; bindings = empty} in
        Ok {rule; work = [rule]}
      | Error err -> Error err

  let of_sexps ~get_pos xs =
    List.map xs ~f:(of_sexp ~get_pos) |>
    Result.all

  (* the parser infrastructure may look overly generalized here,
     but we are planning to share it later with the Primus Lisp
     parser, once we will move it on a newer Parsexp library.*)
  type ('a,'d,'r) parser_ops = {
    parse : string -> ('a, sexp_error) Result.t;
    get_data : 'a -> 'd;
    get_anns : 'a -> positions;
    find_pos : positions -> 'd -> sub:Sexp.t -> range option;
    convert : get_pos:(Sexp.t -> range) -> 'd -> ('r,error) result;
  }

  module type Parser = sig
    type r type a type d
    val ops : (a,d,r) parser_ops
  end

  type 'a parser = (module Parser with type r = 'a)

  let parser (type a d r) (ops : (a,d,r) parser_ops) : r parser =
    let module Parser = struct
      type nonrec r = r
      type nonrec a = a
      type nonrec d = d
      let ops = ops
    end in
    (module Parser)

  let one = parser Parsexp.{
      parse = Single_and_positions.parse_string;
      get_data = fst;
      get_anns = snd;
      find_pos = Positions.find_sub_sexp_phys;
      convert = of_sexp;
    }

  let many = parser Parsexp.{
      parse = Many_and_positions.parse_string;
      get_data = fst;
      get_anns = snd;
      find_pos = Positions.find_sub_sexp_in_list_phys;
      convert = of_sexps;
    }

  let empty_range = Parsexp.Positions.{
      start_pos = beginning_of_file;
      end_pos = beginning_of_file;
    }

  let parse (type t) (module P : Parser with type r = t)  str =
    match P.ops.parse str with
    | Error err -> Error (Not_a_sexp err)
    | Ok r ->
      let pos = P.ops.get_anns r in
      let data = P.ops.get_data r in
      let get_pos sub = match P.ops.find_pos pos data ~sub with
        | None -> empty_range
        | Some x -> x in
      P.ops.convert ~get_pos data

  let from_string str = parse many str
  let from_file name = parse many (In_channel.read_all name)

  let of_string str =
    match parse one str with
    | Ok x -> x
    | Error _  -> failwithf "string %s is not a BARE rule" str ()

  let merge bs bs' =
    Map.fold bs' ~init:(Some bs) ~f:(fun ~key ~data -> function
        | None -> None
        | Some bs -> match Map.find bs key with
          | Some data' when Sexp.equal data data' -> Some bs
          | Some _ -> None
          | None -> Some (Map.add bs ~key ~data))

  let rec bindings lhs rhs = match lhs,rhs with
    | Sexp.Atom x, Sexp.Atom y when x = y -> Some empty
    | Sexp.Atom "?",_ -> Some empty
    | Sexp.Atom x, rhs when is_variable x -> Some (binding x rhs)
    | Sexp.List lhs, Sexp.List rhs -> concat_bindings lhs rhs
    | _ -> None
  and concat_bindings xs ys = match xs,ys with
    | [],[] -> Some empty
    | [], _ :: _ | _ :: _, [] -> None
    | x :: xs, y::ys -> match bindings x y with
      | None -> None
      | Some bs -> match concat_bindings xs ys with
        | None -> None
        | Some bs' -> merge bs bs'

  let rec apply bindings = function
    | Sexp.List xs -> Sexp.List (List.map xs ~f:(apply bindings))
    | Sexp.Atom x when not (is_variable x) -> Sexp.Atom x
    | Sexp.Atom x ->
      match Map.find bindings x with
      | None -> failwithf "internal error - unbound variable %s" x ()
      | Some v -> v

  let apply {bindings; template} =
    List.map template ~f:(apply bindings)

  let split_map xs ~f =
    let rec loop rev_prefix = function
      | [] -> None
      | x :: suffix -> match f ~rev_prefix x ~suffix with
        | Some x -> Some x
        | None -> loop (x::rev_prefix) suffix in
    loop [] xs

  let match_rule fact rule =
    split_map rule.patterns
      ~f:Option.Monad_infix.(fun ~rev_prefix p ~suffix ->
          bindings p fact >>= merge rule.bindings >>| fun bindings -> {
            rule with
            patterns = List.rev_append rev_prefix suffix;
            bindings
          })

  let apply hypot fact =
    List.fold ~init:(hypot.work,[]) hypot.work ~f:(fun (work,acts) r ->
        match_rule fact r |> function
        | Some ({patterns=[]} as m) ->
          work, List.rev_append (apply m) acts
        | Some rule -> rule::work,acts
        | None -> work,acts) |> fun (work,acts) ->
    {hypot with work}, acts

  let patterns {rule} = rule.patterns

  let pp ppf {rule} =
    let pp_sexps = pp_print_list Sexp.pp_hum in
    fprintf ppf "((%a) (%a))"
      pp_sexps rule.patterns
      pp_sexps rule.template

  let spec t = asprintf "%a" pp t

  let pp_range file ppf ({start_pos=s; end_pos=e} : range)  =
    let len = e.offset - s.offset in
    fprintf ppf "File %S, line %d, characters %d-%d:"
      file s.line s.col (s.col+len)


  let report_error ?(filename="//dynamic//") ppf err =
    let pp_range = pp_range filename in
    match err with
    | Unbound (v,r) ->
      fprintf ppf "%a:@\nError: Unbound variable %s@\n" pp_range r v
    | Wildcard r ->
      fprintf ppf "%a:@\nError: Wilcards are not allowed on the right hand side@\n"
        pp_range r
    | Not_a_sexp perr ->
      Parsexp.Parse_error.report ppf ~filename perr
    | Parse_error (Expect_list here,_) ->
      fprintf ppf "%a:@\nParse error: Expected a list, but got an atom"
        pp_range here
    | Parse_error (Expect_pair,here) ->
      fprintf ppf
        "%a:@\nParse error: Expected a list of exactly two elements"
        pp_range here

end
