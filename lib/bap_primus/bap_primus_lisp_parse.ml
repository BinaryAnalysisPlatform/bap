open Core_kernel.Std
open Bap.Std

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Var = Bap_primus_lisp_var
module Word = Bap_primus_lisp_word
module Loc = Bap_primus_lisp_loc
module Resolve = Bap_primus_lisp_resolve

type attribute_error = ..

type defkind = Func | Macro | Const | Subst

type parse_error =
  | Expect_atom
  | Bad_var_literal of Var.read_error
  | Bad_word_literal of Word.read_error
  | Bad_form of string
  | Bad_app
  | Bad_let_binding
  | Bad_param_list
  | Bad_macro_param_list
  | Bad_attribute_syntax
  | Attribute_error of attribute_error
  | Unknown_parser
  | Bad_require
  | Bad_context_value
  | Bad_context_syntax
  | Bad_external_syntax
  | Bad_variables_spec
  | Bad_hook
  | Unknown_toplevel
  | Bad_toplevel
  | Unresolved of defkind * Resolve.resolution

type error += Parse_error of parse_error * tree list
type error += Sexp_error of Source.error
type error += Unresolved_feature of string * Loc.filepos option

type attrs = Attribute.set
type 'a defs = 'a Def.t list

type meta_program = {
  macros : Def.macro defs;
  substs : Def.subst defs;
  consts : Def.const defs;
}

type program = {
  defs : Def.func defs;
  meta : meta_program;
}


let empty_program = {
  defs = [];
  meta={
    macros=[];
    substs=[];
    consts=[];
  }
}

module Parse = struct
  module State = struct
    type t = {
      source : Source.t;
      constraints : Context.t;
      program : program;
    }
  end

  let fails err s = raise (Fail (Parse_error (err,s)))
  let fail err s = fails err [s]
  let bad_form op got = fail (Bad_form op) got
  let nil = {data=0L; typ=Type 1}


  let is_odd x = Int.(x mod 2 = 1)

  let expand {State.program; constraints} cs =
    List.concat_map cs ~f:(function
        | {data=List _} as cs -> [cs]
        | {data=Atom x} as atom ->
          match Resolve.subst program.meta.substs constraints x () with
          | None -> [atom]
          | Some (Error s) -> fail (Unresolved (Subst,s)) atom
          | Some (Ok (d,())) ->
            Def.Subst.body d |> List.map ~f:(fun tree ->
                {tree with id = atom.id}))

  let let_var : tree -> var = function
    | {data=List _} as s -> fail Bad_let_binding s
    | {data=Atom x} as s -> match Var.read x with
      | Error e -> fail (Bad_var_literal e) s
      | Ok var -> var

  let parse ({State.constraints=ctxts} as s) tree =
    let rec exp : tree -> ast = fun tree ->
      let cons data : ast = {data; id=tree.id; eq=tree.eq} in

      let if_ = function
        | c::e1::e2 :: e :: es -> cons (Ite (exp c,exp e1,seq e es))
        | _ -> bad_form "if" tree in

      let let_ = function
        | {data=List bs} :: e :: es ->
          List.fold_right bs ~init:(seq e es) ~f:(fun b e ->
              match b with
              | {data=List [v; x]} -> cons (Let (let_var v,exp x,e))
              | s -> fail Bad_let_binding s)
        | _ -> bad_form "let" tree in

      let while_ = function
        | c :: e :: es -> cons (Rep (exp c, seq e es))
        | _ -> bad_form "while" tree in

      let msg = function
        |  {data=Atom msg} :: es -> cons (Msg (fmt msg, exps es))
        | _ -> bad_form "msg" tree in

      let prog es = cons (Seq (exps es)) in

      let forms = [
        "if", if_;
        "let", let_;
        "prog", prog;
        "while", while_;
        "msg", msg;
      ] in

      let macro op args =
        match Resolve.macro s.program.meta.macros ctxts op args with
        | None -> cons (App (Dynamic op, exps args))
        | Some (Ok (macro,bs)) -> exp (Def.Macro.apply macro bs)
        | Some (Error err) -> fails (Unresolved (Macro,err)) args in

      let list : tree list -> ast = function
        | [] -> cons (Int nil)
        | {data=List _} as s :: _  -> fail Bad_app s
        | {data=Atom op} :: exps ->
          match List.Assoc.find ~equal:String.equal forms op with
          | None -> macro op (expand s exps)
          | Some form -> form exps in

      let var r = match Var.read r with
        | Error e -> fail (Bad_var_literal e) tree
        | Ok v -> cons (Var v) in

      let lit r = match Word.read r with
        | Ok x -> cons (Int x)
        | Error Not_an_int -> var r
        | Error other -> fail (Bad_word_literal other) tree in

      let start : tree -> ast = function
        | {data=List xs} -> list xs
        | {data=Atom x} as t ->
          match Resolve.const s.program.meta.consts ctxts x () with
          | None -> lit x
          | Some Error err -> fail (Unresolved (Const,err)) t
          | Some (Ok (const,())) -> exp (Def.Const.value const) in
      start tree
    and seq e es = {data=Seq ((exp e) :: exps es); id=e.id; eq=e.eq}
    and exps : tree list -> ast list = List.map ~f:exp
    and fmt fmt =
      let nil = `Lit [] in
      let str cs = String.of_char_list (List.rev cs) in
      let push_nothing = ident in
      let push s xs = s :: xs in
      let push_lit s = push (Lit s) in
      let push_pos x = push (Pos (Char.to_int x)) in
      let push_chars cs = push_lit (str cs) in
      let lit parse xs = function
        | '\\' -> parse (push_chars xs) `Esc
        | '$'  -> parse (push_chars xs) `Exp
        | x    -> parse push_nothing (`Lit (x::xs)) in
      let esc parse = function
        | '\\' -> parse (push_lit "\\") nil
        | 'n'  -> parse (push_lit "\n") nil
        | '$'  -> parse (push_lit "$") nil
        | c    -> parse (push_lit (sprintf "\\%c" c)) nil in
      let exp parse = function
        | c when Char.is_digit c -> parse (push_pos c) nil
        | c -> invalid_argf "msg - invalid format got $%c" c () in
      let rec parse off spec state =
        let step push state = parse (off+1) (push spec) state in
        if Int.(off < String.length fmt) then match state with
          | `Lit xs -> lit step xs fmt.[off]
          | `Esc -> esc step fmt.[off]
          | `Exp -> exp step fmt.[off]
        else List.rev @@ match state with
          | `Lit xs -> push_chars xs spec
          | _ -> invalid_argf "invalid format string '%s'" fmt () in
      (parse 0 [] nil) in
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

  open State


  let with_def state def = {
    state with program = {
      state.program with defs = def :: state.program.defs
    }
  }

  let with_macro meta macro = {meta with macros = macro :: meta.macros}
  let with_subst meta subst = {meta with substs = subst :: meta.substs}
  let with_const meta const = {meta with consts = const :: meta.consts}
  let meta state add stuff = {
    state with program = {
      state.program with meta = add state.program.meta stuff
    }
  }

  let parse_declarations attrs =
    List.fold ~init:attrs ~f:Attribute.parse

  let reader = function
    | None -> Def.Subst.Ident
    | Some ":ascii" -> Def.Subst.Ascii
    | Some ":hex" -> Def.Subst.Hex
    | Some unknown ->
      invalid_argf "Unknown substitution syntax %s" unknown ()

  let is_keyarg s = Char.(s.[0] = ':')

  let constrained state attrs =
    match Attribute.Set.get attrs Context.t with
    | None -> state
    | Some constraints -> {state with constraints}

  let defun ?docs ?(attrs=[]) name p b bs state gattrs =
    let attrs = parse_declarations gattrs attrs in
    let es = List.map ~f:(parse (constrained state attrs)) (b::bs) in
    with_def state @@ Def.Func.create ?docs ~attrs name (params p) {
      data = Seq es;
      id = b.id;
      eq = b.eq;
    }

  let defmacro ?docs ?(attrs=[]) name ps body state gattrs =
    meta state with_macro @@
    Def.Macro.create ?docs
      ~attrs:(parse_declarations gattrs attrs) name
      (metaparams ps)
      body

  let defsubst ?docs ?(attrs=[]) ?syntax name body state gattrs =
    meta state with_subst @@
    Def.Subst.create ?docs
      ~attrs:(parse_declarations gattrs attrs) name
      (reader syntax)
      body

  let defconst ?docs ?(attrs=[]) name body state gattrs =
    meta state with_const @@
    Def.Const.create ?docs
      ~attrs:(parse_declarations gattrs attrs) name ~value:body

  let toplevels = String.Set.of_list [
      "declare";
      "defconstant";
      "defmacro";
      "defsubst";
      "defun";
    ]

  let declarations gattrs s = match s with
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
        b :: body)
      } ->
      defun ~docs ~attrs name params b body state gattrs
    | {data = List (
        {data=Atom "defun"} ::
        {data=Atom name} ::
        params ::
        {data=Atom docs} ::
        b :: body)
      } ->
      defun ~docs name params b body state gattrs
    | {data = List (
        {data=Atom "defun"} ::
        {data=Atom name} ::
        params ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        b :: body)
      } ->
      defun ~attrs name params b body state gattrs
    | {data = List (
        {data=Atom "defun"} ::
        {data=Atom name} ::
        params ::
        b :: body)
      } ->
      defun name params b body state gattrs
    | s -> fail Bad_toplevel s


  let meta gattrs state s = match s with
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=Atom docs};
        {data=List ({data=Atom "declare"} :: attrs)};
        {data=Atom body };
      ]} ->
      defconst ~docs ~attrs name body state gattrs
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=Atom docs};
        {data=Atom body };
      ]} ->
      defconst ~docs name body state gattrs
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=List ({data=Atom "declare"} :: attrs)};
        {data=Atom body};
      ]} ->
      defconst ~attrs name body state gattrs
    | {data=List [
        {data=Atom "defconstant"};
        {data=Atom name};
        {data=Atom body };
      ]} ->
      defconst name body state gattrs

    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        {data=Atom docs};
        {data=List ({data=Atom "declare"} :: attrs)};
        body]} ->
      defmacro ~docs ~attrs name params body state gattrs
    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        {data=Atom docs};
        body]} ->
      defmacro ~docs name params body state gattrs
    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        {data=List ({data=Atom "declare"} :: attrs)};
        body]} ->
      defmacro ~attrs name params body state gattrs
    | {data=List [
        {data=Atom "defmacro"};
        {data=Atom name};
        params;
        body]} ->
      defmacro name params body state gattrs

    (* we can't add docstrings to the docstrings as it will make
       grammar ambiguous -- we are unable to distinguish between
       the first value in a series of values, and the docstring,
       since we don't have any specific marker that separates the
       meta description from the body. *)
    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        {data=Atom syntax} ::
        body)} when is_keyarg syntax ->
      defsubst ~attrs ~syntax name body state gattrs
    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        {data=Atom syntax} ::
        body)} when is_keyarg syntax ->
      defsubst ~syntax name body state gattrs
    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        {data=List ({data=Atom "declare"} :: attrs)} ::
        body)} ->
      defsubst ~attrs name body state gattrs
    | {data=List (
        {data=Atom "defsubst"} ::
        {data=Atom name} ::
        body)} ->
      defsubst name body state gattrs

    | _ -> state

  let source constraints source =
    let init = {source; constraints; program=empty_program} in
    Source.fold source ~init ~f:(fun _ trees state ->
        let gattrs =
          List.fold trees ~init:Attribute.Set.empty ~f:declarations in
        let state =
          List.fold trees ~init:state ~f:(meta gattrs) in
        List.fold trees ~init:state ~f:(stmt gattrs))
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

  let tree paths p feature =
    match file_of_feature paths feature with
    | None ->
      raise (Fail (Unresolved_feature (feature,None)))
    | Some name -> match Source.find p name with
      | Some _ -> p
      | None -> match Source.load p name with
        | Ok p -> p
        | Error err -> raise (Fail (Sexp_error err))

  let trees paths p features =
    List.fold ~init:p features ~f:(tree paths)

  let parse_require tree = match tree with
    | {data=List [{data=Atom "require"}; {data=Atom name}]} ->
      Some name
    | {data=List ({data=Atom "require"} :: xs)} ->
      raise (Fail (Parse_error (Bad_require,xs)))
    | _ -> None

  let required paths p =
    Source.fold p ~init:String.Set.empty ~f:(fun _ trees required ->
        List.fold trees ~init:required ~f:(fun required tree ->
            match parse_require tree with
            | None -> required
            | Some name -> match file_of_feature paths name with
              | None ->
                let pos = Source.pos p tree in
                raise (Fail (Unresolved_feature (name,Some pos)))
              | Some file -> match Source.find p file with
                | None -> Set.add required name
                | Some _ -> required))

  let transitive_closure paths p =
    let rec fixpoint p =
      let required = required paths p in
      if Set.is_empty required then p
      else fixpoint (Set.fold required ~init:p ~f:(tree paths)) in
    fixpoint p

  let features paths constraints features =
    trees paths Source.empty features |> transitive_closure paths |>
    Parse.source constraints

end


(* let operators = [ *)
(*    "+" , Add; *)
(*    "-" , Sub; *)
(*    "*" , Mul; *)
(*    "/" , Div; *)
(*    "mod" , Mod; *)
(*    "signed-mod" , Mods; *)
(*    "s/" , Divs; *)
(*    "signed-div" , Divs; *)
(*    "shift-left", Lsl; *)
(*    "shift-right", Lsr; *)
(*    "signed-shift-right", Asr; *)
(*    "=" , Equal; *)
(*    "<" , Less; *)
(*    "and", And; *)
(*    "or", Or; *)
(*    "logand" , Land; *)
(*    "logxor", Lxor; *)
(*    "logior", Lior; *)
(*    "cat", Cat; *)
(* ] *)
