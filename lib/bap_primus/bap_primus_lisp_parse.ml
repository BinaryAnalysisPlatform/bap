open Core_kernel
open Bap.Std
open Format

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Var = Bap_primus_lisp_var
module Word = Bap_primus_lisp_word
module Loc = Bap_primus_lisp_loc
module Resolve = Bap_primus_lisp_resolve
module Program = Bap_primus_lisp_program
module Type = Bap_primus_lisp_type

type defkind = Func | Macro | Const | Subst | Meth | Para

type format_error =
  | Expect_digit
  | Illegal_escape of char
  | Unterminated of [`Esc | `Exp]

type parse_error =
  | Bad_var_literal of Var.read_error
  | Bad_word_literal of Word.read_error
  | Bad_form of string
  | Bad_app
  | Bad_let_binding
  | Bad_param_list
  | Bad_macro_param_list
  | Bad_require
  | Unknown_toplevel
  | Bad_toplevel
  | Bad_def of defkind
  | Bad_ascii
  | Bad_hex
  | Unknown_subst_syntax
  | Unresolved of defkind * string * Resolve.resolution


exception Parse_error of parse_error * tree list
exception Attribute_parse_error of Attribute.error * tree list * tree

type source =
  | Cmdline
  | Module of Loc.t

type error =
  | Parse_error of parse_error * Loc.t
  | Format_error of format_error * Loc.t
  | Sexp_error of string * source * Source.error
  | Unresolved_feature of string * source
  | Unknown_attr of string * Loc.t

exception Fail of error

let loc src trees = match trees with
  | [] -> Source.loc src Id.null
  | t :: ts ->
    List.fold ts ~init:(Source.loc src t.id) ~f:(fun loc t ->
        Loc.merge loc (Source.loc src t.id))

let is_quoted s =
  let n = String.length s in
  n > 1 && s.[0] = '"' && s.[n - 1] = '"'

let is_symbol s =
  String.length s > 1 && s.[0] = '\''

let unqoute s =
  if is_quoted s
  then String.sub ~pos:1 ~len:(String.length s - 2) s
  else s

let symbol s =
  if is_symbol s
  then String.subo ~pos:1 s
  else s

module Parse = struct
  open Program.Items

  let fails err s = raise (Parse_error (err,s))
  let fail err s = fails err [s]
  let bad_form op got = fail (Bad_form op) got
  let nil = {exp=0L; typ=Type.word 1}


  let expand prog cs =
    List.concat_map cs ~f:(function
        | {data=List _} as cs -> [cs]
        | {data=Atom x} as atom ->
          match Resolve.subst prog subst x () with
          | None -> [atom]
          | Some (Error s) -> fail (Unresolved (Subst,x,s)) atom
          | Some (Ok (d,())) ->
            Def.Subst.body d |> List.map ~f:(fun tree ->
                {tree with id = atom.id}))

  let let_var : tree -> var = function
    | {data=List _} as s -> fail Bad_let_binding s
    | {data=Atom x; id; eq} as s -> match Var.read id eq x with
      | Error e -> fail (Bad_var_literal e) s
      | Ok var -> var


  let fmt prog fmt tree =
    let fmt = unqoute fmt in
    let fail err off =
      let pos =
        Loc.nth_char (loc (Program.sources prog) [tree]) (off+1) in
      raise (Fail (Format_error (err, pos))) in
    let unescape off c =
      try Scanf.unescaped (sprintf "\\%c" c)
      with Scanf.Scan_failure _ ->
        fail (Illegal_escape c) off in
    let nil = `Lit [] in
    let str cs = String.of_char_list (List.rev cs) in
    let push_nothing = ident in
    let push s xs = s :: xs in
    let push_lit s = push (Lit s) in
    let push_pos x = push (Pos (Int.of_string (Char.to_string x))) in
    let push_chars cs = push_lit (str cs) in
    let rec parse off spec state =
      let lit parse xs = function
        | '\\' -> parse (push_chars xs) `Esc
        | '$'  -> parse (push_chars xs) `Exp
        | x    -> parse push_nothing (`Lit (x::xs)) in
      let esc parse = function
        | '$'  -> parse (push_lit "$") nil
        | c    -> parse (push_lit (unescape off c)) nil in
      let exp parse = function
        | c when Char.is_digit c -> parse (push_pos c) nil
        | _ -> fail Expect_digit off in
      let step push state = parse (off+1) (push spec) state in
      if Int.(off < String.length fmt) then match state with
        | `Lit xs -> lit step xs fmt.[off]
        | `Esc -> esc step fmt.[off]
        | `Exp -> exp step fmt.[off]
      else List.rev @@ match state with
        | `Lit xs -> push_chars xs spec
        | (`Esc|`Exp) as state -> fail (Unterminated state) off in
    parse 0 [] nil


  let parse prog tree =
    let rec exp : tree -> ast = fun tree ->
      let cons data : ast = {data; id=tree.id; eq=tree.eq} in

      let if_ = function
        | c::e1::e2 :: es -> cons (Ite (exp c,exp e1,seq e2 es))
        | _ -> bad_form "if" tree in

      let let_ = function
        | {data=List bs} :: e :: es ->
          List.fold_right bs ~init:(seq e es) ~f:(fun b e ->
              match b with
              | {data=List [v; x]; id; eq} ->
                {data=Let (let_var v,exp x,e); id; eq}
              | s -> fail Bad_let_binding s)
        | _ -> bad_form "let" tree in

      let while_ = function
        | c :: e :: es -> cons (Rep (exp c, seq e es))
        | _ -> bad_form "while" tree in

      let msg = function
        | {data=Atom msg} as tree :: es when is_quoted msg ->
          cons (Msg (fmt prog msg tree, exps es))
        | _ -> bad_form "msg" tree in

      let set = function
        | [v; e] -> cons (Set (let_var v, exp e))
        | _ -> bad_form "set" tree in

      let prog_ es = cons (Seq (exps es)) in

      let error = function
        | [{data=Atom msg}] when is_quoted msg -> cons (Err msg)
        | _ -> bad_form "error" tree in

      let forms = [
        "if", if_;
        "let", let_;
        "prog", prog_;
        "while", while_;
        "msg", msg;
        "set", set;
        "error", error;
      ] in

      let macro op args = match Resolve.macro prog macro op args with
        | None -> cons (App (Dynamic op, exps args))
        | Some (Ok (macro,bs)) -> exp (Def.Macro.apply macro bs)
        | Some (Error err) -> fail (Unresolved (Macro,op,err)) tree in

      let list : tree list -> ast = function
        | [] -> cons (Int {data=nil; id=tree.id; eq=tree.eq})
        | {data=List _} as s :: _  -> fail Bad_app s
        | {data=Atom op} :: exps ->
          match List.Assoc.find ~equal:String.equal forms op with
          | None -> macro op (expand prog exps)
          | Some form -> form exps in

      let sym ({id;eq;data=r} as s)  =
        if is_symbol r then cons (Sym { s with data = symbol s.data})
        else match Var.read id eq r with
          | Error e -> fail (Bad_var_literal e) tree
          | Ok v -> cons (Var v) in

      let lit ({id; eq; data=r} as t) = match Word.read id eq r with
        | Ok x -> cons (Int x)
        | Error Not_an_int -> sym t
        | Error other -> fail (Bad_word_literal other) tree in

      let start : tree -> ast = function
        | {data=List xs} -> list xs
        | {data=Atom x} as t -> match Resolve.const prog const x () with
          | None -> lit {t with data=x}
          | Some Error err -> fail (Unresolved (Const,x,err)) t
          | Some (Ok (const,())) -> exp (Def.Const.value const) in
      start tree
    and seq e es = {data=Seq ((exp e) :: exps es); id=Id.null; eq=Eq.null}
    and exps : tree list -> ast list = fun xs -> List.map xs ~f:exp in
    exp tree

  let params = function
    | {data=List vars} -> List.map ~f:let_var vars
    | s -> fail Bad_param_list s

  let atom = function
    | {data=Atom x} -> x
    | tree -> fail Bad_macro_param_list tree

  let metaparams = function
    | {data=List vars} -> List.map ~f:atom vars
    | s -> fail Bad_macro_param_list s


  let parse_declaration attrs tree =
    try Attribute.parse attrs tree
    with Attribute.Bad_syntax (err,trees) ->
      raise (Attribute_parse_error (err,trees,tree))

  let parse_declarations attrs =
    List.fold ~init:attrs ~f:Attribute.parse

  let ascii xs =
    let rec loop xs acc = match xs with
      | [] -> acc
      | {data=Atom x} as s :: xs when is_quoted x ->
        let x = try Scanf.unescaped x
          with Scanf.Scan_failure _ -> fail Bad_ascii s in
        String.fold x ~init:acc ~f:(fun acc x ->
            {data=x; id = s.id; eq = Eq.null} :: acc) |>
        loop xs
      | here :: _ -> fail Bad_ascii here in
    List.rev_map (loop xs []) ~f:(fun c ->
        {c with data=Atom (sprintf "%#02x" (Char.to_int c.data))})

  let is_odd x = x mod 2 = 1

  let hex xs =
    let rec loop xs acc = match xs with
      | [] -> List.rev acc
      | {data=List _} as here :: _ -> fail Bad_hex here
      | {data=Atom x} as s :: xs ->
        let x = if is_odd (String.length x) then "0" ^ x else x in
        String.foldi x ~init:acc ~f:(fun i acc _ ->
            if is_odd i
            then {s with data=Atom (sprintf "0x%c%c" x.[i-1] x.[i])} :: acc
            else acc) |>
        loop xs in
    loop xs []

  let reader = function
    | None -> ident
    | Some {data=Atom ":ascii"} -> ascii
    | Some {data=Atom ":hex"} -> hex
    | Some here -> fail Unknown_subst_syntax here

  let is_keyarg = function
    | {data=Atom s} -> Char.(s.[0] = ':')
    | _ -> false

  let constrained prog attrs =
    match Attribute.Set.get attrs Context.t with
    | None -> prog
    | Some constraints ->
      Program.with_context prog @@
      Context.merge (Program.context prog) constraints

  let defun ?docs ?(attrs=[]) name p body prog gattrs tree =
    let attrs = parse_declarations gattrs attrs in
    let es = List.map ~f:(parse (constrained prog attrs)) body in
    Program.add prog func @@ Def.Func.create ?docs ~attrs name (params p) {
      data = Seq es;
      id = tree.id;
      eq = tree.eq;
    } tree

  let defmethod ?docs ?(attrs=[]) name p body prog gattrs tree =
    let attrs = parse_declarations gattrs attrs in
    let es = List.map ~f:(parse (constrained prog attrs)) body in
    Program.add prog meth @@ Def.Meth.create ?docs ~attrs name (params p) {
      data = Seq es;
      id = tree.id;
      eq = tree.eq;
    } tree

  let defmacro ?docs ?(attrs=[]) name ps body prog gattrs tree =
    Program.add prog macro @@
    Def.Macro.create ?docs
      ~attrs:(parse_declarations gattrs attrs) name
      (metaparams ps)
      body tree

  let defparameter ?docs ?(attrs=[]) name body prog gattrs tree =
    let attrs = parse_declarations gattrs attrs in
    Program.add prog para @@
    Def.Para.create ?docs
      ~attrs name (parse (constrained prog attrs) body) tree

  let defsubst ?docs ?(attrs=[]) name body prog gattrs tree =
    let syntax = match body with
      | s :: _ when is_keyarg s -> Some s
      | _ -> None in
    Program.add prog subst @@
    Def.Subst.create ?docs
      ~attrs:(parse_declarations gattrs attrs) name
      (reader syntax body) tree

  let defconst ?docs ?(attrs=[]) name body prog gattrs tree =
    Program.add prog const @@
    Def.Const.create ?docs
      ~attrs:(parse_declarations gattrs attrs) name ~value:body tree

  let toplevels = String.Set.of_list [
      "declare";
      "defconstant";
      "defparameter";
      "defmacro";
      "defsubst";
      "defun";
      "defmethod";
      "require";
    ]

  let declaration gattrs s = match s with
    | {data=List ({data=Atom "declare"} :: attrs)} ->
      parse_declarations gattrs attrs
    | {data=List ({data=Atom toplevel} as here :: _)} ->
      if Set.mem toplevels toplevel then gattrs
      else fail Unknown_toplevel here
    | _ -> fail Bad_toplevel s


  let stmt gattrs state s = match s with
    | {data = List (
        {data=Atom "defun"} ::
        {data=Atom name} ::
        params ::
        {data=Atom docs} ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        body)
      } when is_quoted docs ->
      defun ~docs ~attrs name params body state gattrs s
    | {data = List (
        {data=Atom "defun"} ::
        {data=Atom name} ::
        params ::
        {data=Atom docs} ::
        body)
      } when is_quoted docs ->
      defun ~docs name params body state gattrs s
    | {data = List (
        {data=Atom "defun"} ::
        {data=Atom name} ::
        params ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        body)
      } ->
      defun ~attrs name params body state gattrs s
    | {data = List (
        {data=Atom "defun"} ::
        {data=Atom name} ::
        params ::
        body)
      } ->
      defun name params body state gattrs s
    | {data=List ({data=Atom "defun"} :: _)} -> fail (Bad_def Func) s
    | {data = List (
        {data=Atom "defmethod"} ::
        {data=Atom name} ::
        params ::
        {data=Atom docs} ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        body)
      } when is_quoted docs ->
      defmethod ~docs ~attrs name params body state gattrs s
    | {data = List (
        {data=Atom "defmethod"} ::
        {data=Atom name} ::
        params ::
        {data=Atom docs} ::
        body)
      } when is_quoted docs ->
      defmethod ~docs name params body state gattrs s
    | {data = List (
        {data=Atom "defmethod"} ::
        {data=Atom name} ::
        params ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        body)
      } ->
      defmethod ~attrs name params body state gattrs s
    | {data = List (
        {data=Atom "defmethod"} ::
        {data=Atom name} ::
        params ::
        body)
      } ->
      defmethod name params body state gattrs s
    | {data=List ({data=Atom "defmethod"} :: _)} -> fail (Bad_def Meth) s
    | {data = List [
        {data=Atom "defparameter"};
        {data=Atom name};
        body
      ]} ->
      defparameter name body state gattrs s
    | {data = List [
        {data=Atom "defparameter"};
        {data=Atom name};
        body;
        {data=List ({data=Atom "declare"} :: attrs)}
      ]} ->
      defparameter ~attrs name body state gattrs s
    | {data = List [
        {data=Atom "defparameter"};
        {data=Atom name};
        body;
        {data=Atom docs};
      ]} ->
      defparameter ~docs name body state gattrs s
    | {data = List [
        {data=Atom "defparameter"};
        {data=Atom name};
        body;
        {data=Atom docs};
        {data=List ({data=Atom "declare"} :: attrs)}
      ]} ->
      defparameter ~attrs ~docs name body state gattrs s
    | {data=List ({data=Atom "defparameter"} :: _)} -> fail (Bad_def Para) s
    | _ -> state


  let meta gattrs state s = match s with
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=Atom body};
        {data=Atom docs};
        {data=List ({data=Atom "declare"} :: attrs)};
      ]} when is_quoted docs ->
      defconst ~docs ~attrs name body state gattrs s
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=Atom body };
        {data=Atom docs};
      ]} when is_quoted docs ->
      defconst ~docs name body state gattrs s
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=Atom body};
        {data=List ({data=Atom "declare"} :: attrs)};
      ]} ->
      defconst ~attrs name body state gattrs s
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=Atom body };
      ]} ->
      defconst name body state gattrs s

    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        {data=Atom docs};
        {data=List ({data=Atom "declare"} :: attrs)};
        body]} when is_quoted docs ->
      defmacro ~docs ~attrs name params body state gattrs s
    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        {data=Atom docs};
        body]} when is_quoted docs ->
      defmacro ~docs name params body state gattrs s
    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        {data=List ({data=Atom "declare"} :: attrs)};
        body]} ->
      defmacro ~attrs name params body state gattrs s
    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        body]} ->
      defmacro name params body state gattrs s

    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        {data=Atom docs} ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        body)} when is_quoted docs ->
      defsubst ~docs ~attrs name body state gattrs s
    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        {data=Atom docs} ::
        body)} when is_quoted docs ->
      defsubst ~docs name body state gattrs s
    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        body)} ->
      defsubst ~attrs name body state gattrs s
    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        body)} ->
      defsubst name body state gattrs s
    | {data=List ({data=Atom "defsubst"}::_)} -> fail (Bad_def Subst) s
    | {data=List ({data=Atom "defmacro"}::_)} -> fail (Bad_def Macro) s
    | {data=List ({data=Atom "defconst"}::_)} -> fail (Bad_def Const) s
    | _ -> state

  let declarations =
    List.fold ~init:Attribute.Set.empty ~f:declaration

  let source constraints source =
    let init = Program.with_context Program.empty constraints in
    let init = Program.with_sources init source in
    let state = Source.fold source ~init ~f:(fun _ trees state ->
        List.fold trees ~init:state ~f:(meta (declarations trees))) in
    Source.fold source ~init:state ~f:(fun _ trees state ->
        List.fold trees ~init:state ~f:(stmt (declarations trees)))
end

module Load = struct
  let file_of_feature paths feature =
    let name = feature ^ ".lisp" in
    List.find_map paths ~f:(fun path ->
        Sys.readdir path |> Array.find_map ~f:(fun file ->
            if String.(file = name)
            then Some (Filename.concat path file)
            else None))


  let is_loaded p name = Option.is_some (Source.find p name)

  let load_tree paths p feature loc =
    match file_of_feature paths feature with
    | None ->
      raise (Fail (Unresolved_feature (feature,loc)))
    | Some name -> match Source.find p name with
      | Some _ -> p
      | None -> match Source.load p name with
        | Ok p -> p
        | Error err -> raise (Fail (Sexp_error (feature,loc,err)))

  let load_trees paths p features =
    Map.fold ~init:p features ~f:(fun ~key ~data p ->
        load_tree paths p key data)

  let parse_require tree = match tree with
    | {data=List [{data=Atom "require"}; {data=Atom name}]} ->
      Some (Ok name)
    | {data=List ({data=Atom "require"} :: xs)} -> Some (Error xs)
    | _ -> None

  let required paths p =
    Source.fold p ~init:String.Map.empty ~f:(fun _ trees required ->
        List.fold trees ~init:required ~f:(fun required tree ->
            match parse_require tree with
            | None -> required
            | Some (Error trees) ->
              raise (Fail (Parse_error (Bad_require,loc p trees)))
            | Some (Ok name) ->
              let pos = Module (Source.loc p tree.id) in
              match file_of_feature paths name with
              | None -> raise (Fail (Unresolved_feature (name,pos)))
              | Some file -> match Source.find p file with
                | None -> Map.set required name pos
                | Some _ -> required))

  let transitive_closure paths p =
    let rec fixpoint p =
      let required = required paths p in
      if Map.is_empty required then p
      else fixpoint (load_trees paths p required) in
    fixpoint p

  let features_of_list =
    List.fold ~init:String.Map.empty ~f:(fun fs f ->
        Map.set fs ~key:f ~data:Cmdline)

  let features ?(paths=[Filename.current_dir_name]) proj fs =
    let source =
      load_trees paths Source.empty (features_of_list fs) |>
      transitive_closure paths in
    try
      Parse.source (Context.of_project proj) source
    with Parse_error (err,trees) ->
      raise (Fail (Parse_error (err, loc source trees)))
end

let program ?paths proj features =
  try Ok (Load.features ?paths proj features)
  with Fail e -> Error e

let string_of_var_error = function
  | Var.Empty -> "empty string can't be used as a variable name"
  | Var.Not_a_var -> "not a valid identifier"
  | Var.Bad_type -> "variable type should be a decimal number"
  | Var.Bad_format -> "variable name contains extra `:' symbol"

let string_of_word_error = function
  | Word.Empty -> "an empty string"
  | Word.Not_an_int -> "doesn't start with a number"
  | Word.Bad_literal ->
    "must start with a digit and contain no more than one `:' symbol"
  | Word.Unclosed -> "unmatching single quote in a character literal"
  | Word.Bad_type -> "the type annotation should be a decimal number"

let string_of_form_syntax = function
  | "if" -> "(if <exp> <exp> <exp> ...)"
  | "let" -> "(let ((<var> <exp>) ...) <exp> ...)"
  | "while" -> "(while <exp> <exp> <exp> ...)"
  | "msg" -> "(msg <fmt> <exp> ...)"
  | "set" -> "(set <var> <exp>)"
  | "prog" -> "(prog <exp> ...)"
  | "error" -> {|(error "<msg>")|}
  | _ -> assert false

let string_of_defkind = function
  | Func -> "function"
  | Meth -> "method"
  | Para -> "parameter"
  | Macro -> "macro"
  | Const -> "constant"
  | Subst -> "substitution"


let string_of_def_syntax = function
  | Func  -> "(defun <ident> (<var> ...) [<docstring>] [<declarations>] <exp> ...)"
  | Meth  -> "(defmethod <ident> (<var> ...) [<docstring>] [<declarations>] <exp> ..."
  | Para  -> "(defparameter <ident> <exp> [<docstring>] [<declarations>])"
  | Macro -> "(defmacro <ident> (<ident> ...) [<docstring>] [<declarations>] <exp>)"
  | Const -> "(defconstant <ident> [<docstring>] [<declarations>] <atom>)"
  | Subst -> "(defsubst <ident> [<docstring>] [<declarations>] [:<syntax>] <atom> ...)"

let pp_parse_error ppf err = match err with
  | Bad_var_literal e ->
    fprintf ppf "bad variable literal - %s" (string_of_var_error e)
  | Bad_word_literal e ->
    fprintf ppf "bad word literal - %s" (string_of_word_error e)
  | Bad_form name ->
    fprintf ppf "bad %s syntax - expected %s" name (string_of_form_syntax name)
  | Bad_app ->
    fprintf ppf "head of the list in the application form shall be an atom"
  | Bad_let_binding ->
    fprintf ppf "expected a variable literal"
  | Bad_param_list ->
    fprintf ppf "expected a list of variables"
  | Bad_macro_param_list ->
    fprintf ppf "expected a list of atoms"
  | Bad_require ->
    fprintf ppf "expected (require <feature>)"
  | Unknown_toplevel ->
    fprintf ppf "unknown toplevel form"
  | Bad_toplevel ->
    fprintf ppf "bad toplevel syntax"
  | Bad_ascii | Bad_hex ->
    fprintf ppf "expected a list of atoms"
  | Unknown_subst_syntax ->
    fprintf ppf "unknown substitution syntax"
  | Unresolved (k,n,r) ->
    fprintf ppf "unable to resolve %s `%s', because %a"
      (string_of_defkind k) n Resolve.pp_resolution r
  | Bad_def def ->
    fprintf ppf "bad %s definition, expect %s"
      (string_of_defkind def) (string_of_def_syntax def)

let pp_request ppf req = match req with
  | Cmdline ->
    fprintf ppf "requested by a user"
  | Module loc ->
    fprintf ppf "requested in %a" Loc.pp loc

let pp_format_error ppf err = match err with
  | Expect_digit -> fprintf ppf "expected digit"
  | Unterminated `Esc -> fprintf ppf "unterminated escape sequence"
  | Unterminated `Exp -> fprintf ppf "unterminated reference"
  | Illegal_escape c  -> fprintf ppf "illegal escape character '%c'" c

let pp_error ppf err = match err with
  | Parse_error (err,loc) ->
    fprintf ppf "%a@\nParse error: %a" Loc.pp loc pp_parse_error err
  | Sexp_error (name,req,err) ->
    fprintf ppf "%a@\nOccured when parsing feature %s %a"
      Source.pp_error err name pp_request req
  | Unresolved_feature (name,req) ->
    fprintf ppf "Error: no implementation provided for feature `%s' %a"
      name pp_request req
  | Unknown_attr (attr,loc) ->
    fprintf ppf "%a@\nError: unknown attribute %s@\n" Loc.pp loc attr
  | Format_error (err,loc) ->
    fprintf ppf "%a@\nFormat error: %a" Loc.pp loc pp_format_error err

let pp_program = Program.pp
