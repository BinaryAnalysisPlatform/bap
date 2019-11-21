open Core_kernel
open Regular.Std
open Bap.Std
open Bap_bml
open Format

include Self()

let grammar = {|
    bml    ::= (<exps> <exps>)
    exps   ::= <exp>  | (<exp>1 .. <exp>N)
    exp    ::= (<id>) | (<id> <arg>)
    arg    ::= <id> | ?quoted string?
    id     ::= ?alphanumeric sequence?
|}

module Scheme = struct
  open Sexp.O

  type pred = bool Term.visitor
  type mark = Term.mapper
  type patt = pred list * mark list
  type t = patt list

  let error fmt = ksprintf (fun s -> Error s) fmt
  let unbound_name name = error "Unbound name: %S" name
  let expect_arg n name = error "Term %s has arity %d" name n

  let lookup arity ns1 ns2 tag = match ns1 tag with
    | Some exp -> Ok exp
    | None -> match ns2 tag with
      | None -> unbound_name tag
      | Some exp -> expect_arg arity tag

  let lookup0 nns uns = lookup 0 nns uns
  let lookup1 nns uns = lookup 1 uns nns

  let parse_exp0 = lookup0
  let parse_exp1 uns nns tag v = match lookup1 uns nns tag with
    | Error err -> Error err
    | Ok parse_arg -> try Ok (parse_arg v) with
      | Parse_error msg -> Error msg

  let rec parse_exp nns uns = function
    | List [Atom tag] -> parse_exp0 nns uns tag
    | List [Atom tag; Atom v] -> parse_exp1 nns uns tag v
    | list -> error "expected <exp> got %s" @@ Sexp.to_string list

  let rec parse_exps nns uns = function
    | List (Atom _ :: _) as exp -> [parse_exp nns uns exp]
    | List exps ->
      List.map exps ~f:(parse_exps nns uns) |> List.concat
    | s -> [error "expected <expr> got %s" @@ Sexp.to_string s]

  let parse_mappers s =
    parse_exps Mappers.Nullary.find Mappers.Unary.find s |>
    Result.all

  let parse_preds s =
    parse_exps Predicates.Nullary.find Predicates.Unary.find s |>
    Result.all

  let parse_marker ps ms = match parse_preds ps, parse_mappers ms with
    | Ok ps, Ok ms -> Ok (ps,ms)
    | Error e,_|_,Error e -> Error e

  let parse : sexp -> (patt,string) Result.t = function
    | Sexp.List [preds; marks] -> parse_marker preds marks
    | _ -> Error {|expect "("<preds> <marks>")"|}

  let sexp_error (err : Sexp.parse_error) =
    let line, col = match err.parse_state with
      | `Annot {parse_pos} -> parse_pos.text_line, parse_pos.text_char
      | `Sexp {parse_pos} -> parse_pos.text_line, parse_pos.text_char
    in
    Error (sprintf "Syntax error: line %d, column %d - %s" line col err.err_msg)

  let parse_string s =
    try parse (Sexp.of_string s)
    with Sexp.Parse_error err -> sexp_error err
       | exn -> Error "Malformed sexp"

  let parse_file f =
    try List.map ~f:parse (Sexp.load_sexps f) |> Result.all
    with Sexp.Parse_error err -> sexp_error err
       | Sys_error e -> Error e
       | exn -> Error "Malformed sexp"

  let parse_arg s = match parse_string s with
    | Ok r -> `Ok r
    | Error e -> `Error e

  let arg = Config.converter parse_arg (fun ppf _ -> ()) ([],[])
end

class marker (patts : Scheme.t) = object(self)
  inherit Term.mapper as super

  method! map_term cls t =
    List.fold patts ~init:t ~f:(fun t (preds,maps) ->
        if List.for_all preds ~f:(fun p -> p#enter_term cls t false)
        then List.fold maps ~init:t ~f:(fun t m -> m#map_term cls t)
        else t) |>
    super#map_term cls
end

let unmarker attr = object
  inherit Term.mapper as super

  method! map_term cls t =
    let attrs =
      Dict.filter
        ~f:(fun v -> String.((Value.tagname v) <> attr)) (Term.attrs t) in
    Term.with_attrs t attrs
end

let () =
  Bap_main.Extension.declare @@ fun _ctxt ->
  Mappers.Unary.register "unset-attr" unmarker;
  Map_terms_features.init ();
  Ok ()

let main patts file proj =
  let patts = match file with
    | None -> patts
    | Some file -> match Scheme.parse_file file with
      | Ok ps -> patts @ ps
      | Error err -> raise (Parse_error err) in
  let marker = new marker patts in
  Project.with_program proj @@
    marker#run (Project.program proj)

module Cmdline = struct

  let scheme : Scheme.t Config.param =
    let doc = "Map terms according the $(docv)" in
    Config.(param_all Scheme.arg "with" ~docv:"PATTERN" ~doc)

  let file : string option Config.param =
    let doc = "Read patterns from the $(docv)" in
    Config.(param (some file) "using" ~docv:"FILE" ~doc)

  let bold = List.map ~f:(sprintf "$(b,%s)")

  let term = ["synthetic"; "live"; "dead"; "visited"]
  let sub = [
    "const"; "pure"; "stub"; "extern"; "leaf"; "malloc";
    "noreturn"; "return_twice"; "nothrow"
  ]
  let arg = ["alloc-size"; "restricted"; "nonnull"]

  let enum (xs:string list) : string =
    let xs = bold xs |> List.map ~f:(fun s -> s,()) in
    Config.doc_enum ~quoted:false xs

  let attr attrs name desc =
    `I (sprintf "$(b,(%s))" name,
        sprintf ("%s, where $(i,ATTR) must be one of %s.")
          desc (enum attrs))

  let colors = [
      "black"; "red"; "green"; "yellow"; "blue"; "magenta"; "cyan";
      "white"; "gray"
    ]

  module Predicates = struct

    let color attr =
      `I (sprintf "$(b,(has-%s COLOR))" attr,
          sprintf "Is satisfied when a term's
    attribute $(b,%s) has the given value, where $(i,COLOR) must be
    one of %s" attr (enum colors))

    let term_attr attr =
      `I (sprintf "$(b,(term-%s VALUE))" attr,
          sprintf "Is satisfied when a term's
    attribute $(b,%s) has the given value" attr)

    let term_field attr =
      `I (sprintf "$(b,(term-%s VALUE))" attr,
          sprintf "Is satisfied when a term's
    $(b,%s) has the given value" attr)

    let term_parent =
      `I (sprintf "$(b,(term-parent name))",
          sprintf "Is satisfied when a term is a parent for term with a given name")

    let def_lhs =
      `I (sprintf "$(b,(def-lhs VAR))",
          sprintf "Is satisfied when a term defines $(b,VAR)")

    let def_uses =
      `I (sprintf "$(b,(def-uses VAR))",
          sprintf "Is satisfied when a term uses $(b,VAR)")

    let call =
      `I (sprintf "$(b,(call DST))",
          sprintf "Is satisfied when call of $(b,DST) occurs")

    let goto =
      `I (sprintf "$(b,(goto LABEL))",
          sprintf "Is satisfied when goto $(b,LABEL) occurs")

    let call_return =
      `I (sprintf "$(b,(return DST))",
          sprintf "Is satisfied when call returns to $(b,DST)")

    let section = [
      `S "STANDARD PREDICATES";
      `I ("$(b,(true))","Is always satisfied.");
      attr term "is-ATTR"
        "Is satisfied when a term has the given attribute";
      attr sub "is-ATTR-sub"
        "Is satisfied when a term is a subroutine with the given attribute";
      attr arg "is-ATTR-arg"
        "Is satisfied when a term is an argument with the given attribute";
      `I ("$(b,(has-mark))", "Is satisfied when a term has an
      attribute $(b,mark).");
      color "color";
      color "foreground";
      color "background";
      term_attr  "addr";
      term_field "tid";
      term_field "name";
      term_parent;
      def_lhs;
      def_uses;
      `I ("$(b,(taints))", "Is satisfied if a term is taint source, i.e., has
      $(b,tainted-reg) or $(b,tainted-ptr) attributes.");
      `I ("$(b,(taints-reg))", "Is satisfied if a term is taint source,
      that taints a value stored in a register, i.e., has a
      $(b,tainted-reg) attribute.");
      `I ("$(b,(taints-ptr))", "Is satisfied if a term is taint source,
      that taints a value pointed by a value stored in a register, i.e., has a
      $(b,tainted-ptr) attribute.");
      `I ("$(b,(has-taints))", "Is satisfied if a term is tainted, i.e., has
      $(b,tainted-reg) or $(b,tainted-ptr) attributes.");
      `I ("$(b,(has-tainted-reg))", "Is satisfied if a term uses a
      tainted value stored in a register, i.e., has a
      $(b,tainted-regs) attribute.");
      `I ("$(b,(has-tainted-reg taint))", "Is satisfied if a term uses a
      value tainted with $(i,taint) and stored in a register, where $(i,taint)
      must be a valid taint identifier, e.g., $(b,%12).");
      `I ("$(b,(has-tainted-ptr))", "Is satisfied if a term loads a
      value from a tainted address, i.e., has a $(b,tainted-regs) attribute.");
      `I ("$(b,(has-tainted-reg taint))", "Is satisfied if a term
      loads a value from an address tainted by the give
      $(i,taint). The $(i,taint) must be a valid taint identifier, e.g., $(b,%42).");
    ]
  end

  module Mappers = struct
    let color attr =
      `I (sprintf "$(b,(%s COLOR))" attr,
          sprintf "Set term's attribute $(b,%s) to the given value,
          where $(i,COLOR) must be one of %s" attr (enum colors))

    let section = [
      `S "STANDARD MAPPERS";
      attr term "set-ATTR" "Mark a term with the specified attribute";
      attr sub "set-ATTR-sub" "Mark a term with the specified attribute";
      attr arg "set-ATTR-arg" "Mark a term with the specified attribute";
      `I ("$(b,(set-mark))", "Attch $(b,mark) attribute to a term");
      color "color";
      color "foreground";
      color "background";
      `I ("$(b,(taint-reg TID))", "Mark a term with the given $(b,TID)
      as a taint source for register values.");
      `I ("$(b,(taint-ptr TID))", "Mark a term with the given $(b,TID)
      as a taint source for memory values.");
      `I ("$(b,(unset-ATTR))",
          "Unmark a term from attribute $(b,ATTR) e.g.
        $(b,unset-visited), $(b,unset-foreground)")
    ]
  end

  let grammar = [
    `S "LANGUAGE GRAMMAR";
    `Pre grammar;
  ]

  let example = [
    `S "EXAMPLES";
    `P "$(b,bap) exe --$(mname)$(b,-with)='((is-visited) (foreground blue))'";
    `P {|$(b,bap) exe --$(mname)$(b,-with)='((taints-ptr %12) (comment "ha ha"))'|};
    `P "$(b,bap) exe --$(mname)$(b,-with)='((term-name @strlen) (foreground blue))'";
    `P "$(b,bap) exe --$(mname)$(b,-with)='((term-tid %0000042) (foreground blue))'";
    `P "$(b,bap) exe --$(mname)$(b,-with)='((goto mem[0x42]) (foreground blue))'";
  ]

  let see_also = [
    `S "SEE ALSO"; `P "$(b,bap-plugin-taint)(1), $(b,bap-bml)(3)"
  ]

  let man = [
    `S "SYNOPSIS";
    `P "$(b,bap) [$(b,--)$(mname)$(b,-with)=$(i,SCHEME)]
                 [$(b,--)$(mname)$(b,-using)=$(i,FILE)] $(b,--)$(mname)";
    `S "DESCRIPTION";
    `P "Transform terms using a domain specific pattern matching language.
    The pass accepts a list of patterns via a command line argument
    $(b,--)$(mname)$(b,-with) (that can be specified several times), or
    via file, that contains a list of patterns. Each pattern is
    represented by a pair $(b,(<condition> <action>)). The $(b,<action>) specifies
    a transformation over a term, that is applied if a $(b,<condition>)
    is satisfied. Both $(b,<condition>) and $(b,<action>) can be a
    single $(b,<expression>) or a list of expressions, delimited with
    parentheses. If there is a list of conditions, then all must be
    satisfied. If there is a list of actions, then all actions are
    applied in order. Each expression is either a nullary
    function $(b,(<id>)) or an unary function $(b,(<id>
    <arg>)). Where $(b,<id>) must be a valid predicate or mapper
    name. There is a predefined set of standard functions, but it can
    be extended by adding new mappers or predicates to the BML
    language using $(b, bap-bml) library. ";
  ] @
    Predicates.section @
    Mappers.section @
    grammar @
    example @
    see_also

  let () =
    Config.manpage man;
    Config.when_ready (fun {Config.get=(!)} ->
        try Project.register_pass (main !scheme !file) with
        | Parse_error msg -> eprintf "Parsing error: %s\n%!" msg)
end
