open Core_kernel.Std
open Bap.Std

open Bap_primus_lisp_types

module Attribute = Bap_primus_lisp_attribute
module Context = Bap_primus_lisp_context
module Def = Bap_primus_lisp_def
module Var = Bap_primus_lisp_var
module Word = Bap_primus_lisp_word

type error = ..

type error +=
  | Expect_atom
  | Bad_var_literal of Var.read_error
  | Bad_word_literal of Word.read_erro
  | Bad_form of string
  | Bad_app
  | Bad_let_binding
  | Bad_param_list
  | Bad_macro_param_list
  | Bad_attribute_syntax
  | Unknown_parser
  | Bad_context_value
  | Bad_context_syntax
  | Bad_external_syntax
  | Bad_variables_spec
  | Bad_hook


exception Error of error * sexp

type attrs = Attribute.set
type 'a defs = 'a Def.t list

type program = {
  modules : String.Set.t;
  defs : Def.func defs;
  macros : Def.macro defs;
  substs : Def.subst defs;
}


let error kind where = raise (Error (kind,where))

module Variable = struct
  type t = var
  let parse x = match String.split x ~on:':' with
    | [x;sz] -> {data=x; typ = Type_annot.parse sz}
    | _ -> {data=x;typ=Word}
end


module Parse = struct
  module State = struct
    type t = {
      global_attrs : attrs;
      parse_module : string -> t -> t;
      program : program;
      getpos : Sexp.t -> epos;
      constraints : Context.t;
    }
  end

  let is_word x =
    String.length x > 0 && (x.[0] = '?' || Char.is_digit x.[0])

  let atom = function
    | Atom x -> x
    | s -> parse_error Expect_atom s

  let typ = Type_annot.parse





  let var = Variable.parse
  let int x = int_of_string (atom x)

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

  let bad_form op got = parse_error (Bad_form op) got

  let keywords = [
    "if"; "let"; "neg"; "not";
    "coerce"; "msg"; "while";"set"
  ]
  let is_keyword op = List.mem ~equal:String.equal keywords op
  let nil = {data=0L; typ=Word}

  let ascii xs =
    let rec loop xs acc = match xs with
      | [] -> acc
      | Atom x :: xs ->
        String.fold x ~init:acc ~f:(fun acc x -> x :: acc) |>
        loop xs
      | List _ :: _ ->
        failwith "ascii syntax must contain only atoms" in
    List.rev_map (loop xs []) ~f:(fun c ->
        Atom (sprintf "%#02x" (Char.to_int c)))


  let is_odd x = Int.(x mod 2 = 1)

  let hex xs =
    let rec loop xs acc = match xs with
      | [] -> List.rev acc
      | List _ :: _ -> failwith "hex-data must contain only atoms"
      | Atom x :: xs ->
        let x = if is_odd (String.length x) then "0" ^ x else x in
        String.foldi x ~init:acc ~f:(fun i acc _ ->
            if is_odd i
            then (Atom (sprintf "0x%c%c" x.[i-1] x.[i])) :: acc
            else acc) |>
        loop xs in
    loop xs []

  let expand s cs =
    List.concat_map cs ~f:(function
        | List _ as cs -> [cs]
        | Atom x as atom -> match find_subst s x with
          | None -> [atom]
          | Some subst -> subst.code.elts)


  let char s = {
    data = Int64.of_int (Char.to_int (Char.of_string s));
    typ = Type 8
  }


  let rec parse ({State.constraints=ctxts} as s) sexp =
    let rec exp sexp =
      let cons exp = {pos = s.getpos sexp; arg = exp} in
      let seq es = Seq (cons (exps es)) in

      let if_ = function
        | c::e1::es -> Ite (cons (exp c,exp e1,seq es))
        | _ -> bad_form "if" sexp in

      let let_ = function
        | List bs :: e ->
          List.fold_right bs ~init:(seq e) ~f:(fun b e ->
            match b with
            | List [Atom v; x] -> Let (cons (var v,exp x,e))
            | s -> parse_error Bad_let_binding s)
        | _ -> bad_form "let" sexp in

      let while_ = function
        | c :: es -> Rep (cons (exp c, seq es))
        | _ -> bad_form "while" sexp in

      let msg = function
        |  Atom msg :: es -> Msg (cons (fmt msg, exps es))
        | _ -> bad_form "msg" sexp in

      let forms = [
        "if", if_;
        "let", let_;
        "prog", seq;
        "while", while_;
        "msg", msg;
      ] in

      let apply_macro macro bs =
          let {def} = s.getpos sexp in
          let s = {s with getpos = fun _ ->
              {def; src=Macro macro.meta.loc}} in
          parse s (Macro.apply macro bs) in

      let macro op args =
        match Resolve.macro ctxts s.program.macros op args with
        | res,None -> App (cons (op, exps args))
        | _, Some (macro,bs) -> apply_macro macro bs in

      let list : sexp list -> exp = function
        | [] -> Int (cons nil)
        | List _ as s :: _  -> parse_error Bad_app s
        | Atom op :: exps ->
          match List.Assoc.find ~equal:String.equal forms op with
          | None -> macro op (expand s exps)
          | Some form -> form exps in

      let word x =
        if Char.(x.[0] = '?')
        then Int (cons (char (String.subo ~pos:1 x)))
        else match String.split x ~on:':' with
          | [x] ->  Int (cons {data=Int64.of_string x; typ=Word})
          | [x;sz] -> Int (cons {
              data = Int64.of_string (String.strip x);
              typ   = typ sz
            })
          | _ -> parse_error Bad_word_literal sexp in

      let start : sexp -> exp = function
        | List xs -> list xs
        | Atom x -> match Resolve.macro ctxts s.program.macros x [] with
          | _,None | _,Some ({code={const=false}},_) ->
            if is_word x then word x else Var (cons (var x))
          | _, Some (m,bs) -> apply_macro m bs in
      start sexp
    and exps = List.map ~f:exp
    and fmt fmt =
      let nil = `Lit [] in
      let str cs = String.of_char_list (List.rev cs) in
      let is_atm c = Char.(c = '-' || is_alphanum c) in
      let push_nothing = ident in
      let push s xs = s :: xs in
      let push_lit s = push (Lit s) in
      let push_pos x = push (Pos (Char.to_int x)) in
      let push_chars cs = push_lit (str cs) in
      let push_exp str = push (Exp (exp (Sexp.of_string str))) in
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
        | '(' as c -> parse push_nothing (`Lst ([c],1))
        | c when Char.is_digit c -> parse (push_pos c) nil
        | c when Char.is_alpha c -> parse push_nothing (`Atm [c])
        | c -> invalid_argf "msg - invalid format got $%c" c () in
      let atm parse xs = function
        | c when is_atm c -> parse push_nothing (`Atm (c::xs))
        | c -> parse (push_exp (str xs)) (`Lit [c]) in
      let lst parse xs (lev : int) = function
        | '(' as c -> parse push_nothing (`Lst (c::xs,lev+1))
        | ')' as c -> if Int.(lev = 1)
          then parse (push_exp (str (c::xs))) nil
          else parse push_nothing (`Lst (c::xs,lev-1))
        | c -> parse push_nothing (`Lst (c::xs, lev)) in
      let rec parse off spec state =
        let step push state = parse (off+1) (push spec) state in
        if Int.(off < String.length fmt) then match state with
          | `Lit xs -> lit step xs fmt.[off]
          | `Esc -> esc step fmt.[off]
          | `Exp -> exp step fmt.[off]
          | `Atm xs -> atm step xs fmt.[off]
          | `Lst (xs,lev) -> lst step xs lev fmt.[off]
        else List.rev @@ match state with
          | `Lit xs -> push_chars xs spec
          | `Atm xs -> push_exp (str xs) spec
          | _ -> invalid_argf "invalid format string '%s'" fmt () in
      parse 0 [] nil in
    exp sexp

  let params = function
    | List vars -> List.map ~f:(fun x -> var (atom x)) vars
    | s -> parse_error Bad_param_list s

  let metaparams = function
    | List vars -> List.map ~f:atom vars
    | s -> parse_error Bad_macro_param_list s

  open State


  let add_def prog def = {prog with defs = def :: prog.defs}
  let add_macro prog macro = {prog with macros = macro :: prog.macros}
  let add_subst prog subst = {prog with substs = subst :: prog.substs}
  let parse_declarations attrs =
    List.fold ~init:attrs ~f:Attrs.parse

  let reader = function
    | None -> ident
    | Some ":ascii" -> ascii
    | Some ":hex" -> hex
    | Some unknown ->
      invalid_argf "Unknown substitution syntax %s" unknown ()

  let is_keyarg s = Char.(s.[0] = ':')

  let defun ?(docs="") ?(attrs=[]) name p body state sexp = {
    state with
    program = add_def state.program {
        meta = {
          name;
          docs;
          attrs = parse_declarations state.global_attrs attrs;
          loc = Filepos (state.getpos sexp);
        };
        code = {
          args = params p;
          body = Seq {
              arg = List.map ~f:(parse state) body;
              pos = state.getpos sexp;
          }
        }
      }
  }

  let defmacro ?(docs="") ?(attrs=[]) ?(const=false)
      name ps body state sexp = {
    state with
    program = add_macro state.program {
        meta = {
          name;
          docs;
          attrs = parse_declarations state.global_attrs attrs;
          loc = Filepos (state.getpos sexp);
        };
        code = {
          param = metaparams ps;
          subst = body;
          const;
        }
      }
  }

  let defsubst ?(attrs=[]) ?syntax name body state sexp = {
    state with
    program = add_subst state.program {
        meta = {
          name;
          docs="";
          attrs = parse_declarations state.global_attrs attrs;
          loc = Filepos (state.getpos sexp);
        };
        code = {
          elts = reader syntax body
        }
      }
  }

  let add_advice ~advisor ~advised where state sexp = {
    state with
    program = {
      state.program with
      advices = String.Map.add_multi
          state.program.advices ~key:advised ~data:(where,advisor)
    }
  }

  let hook = function
    | Atom ":before" -> `before
    | Atom ":after" -> `after
    | s -> parse_error Bad_hook s


  let stmt state s = match s with
    | List (Atom "defun" ::
            Atom name ::
            params ::
            Atom docs ::
            List (Atom "declare" :: attrs) ::
            body) ->
      defun ~docs ~attrs name params body state s
    | List (Atom "defun" ::
            Atom name ::
            params ::
            List (Atom "declare" :: attrs) ::
            body) ->
      defun ~attrs name params body state s
    | List (Atom "defun" ::
            Atom name ::
            params ::
            Atom const :: []) ->
      defun name params [Atom const] state s
    | List (Atom "defun" ::
            Atom name ::
            params ::
            Atom docs ::
            body) ->
      defun ~docs name params body state s
    | List (Atom "defun" ::
            Atom name ::
            params ::
            body) ->
      defun name params body state s
    | List [Atom "defconstant";
            Atom name;
            Atom docs;
            List (Atom "declare" :: attrs);
            Atom _ as value;
           ] ->
      defmacro ~docs ~attrs ~const:true name (List []) value state s
    | List [Atom "defconstant";
            Atom name;
            List (Atom "declare" :: attrs);
            Atom _ as value;
           ] ->
      defmacro ~attrs ~const:true name (List []) value state s
    | List [Atom "defconstant";
            Atom name;
            Atom docs;
            Atom _ as value;
           ] ->
      defmacro ~docs ~const:true name (List []) value state s
    | List [Atom "defconstant";
            Atom name;
            Atom _ as value;
           ] ->
      defmacro ~const:true name (List []) value state s
    | List [Atom "defmacro";
            Atom name;
            params;
            Atom docs;
            List (Atom "declare" :: attrs);
            body] ->
      defmacro ~docs ~attrs name params body state s
    | List [Atom "defmacro";
            Atom name;
            params;
            List (Atom "declare" :: attrs);
            body] ->
      defmacro ~attrs name params body state s
    | List [Atom "defmacro";
            Atom name;
            params;
            Atom docs;
            body] ->
      defmacro ~docs name params body state s
    | List [Atom "defmacro";
            Atom name;
            params;
            body] ->
      defmacro name params body state s
    | List (Atom "defsubst" ::
            Atom name ::
            List (Atom "declare" :: attrs) ::
            Atom syntax :: body) when is_keyarg syntax ->
      defsubst ~attrs ~syntax name body state s
    | List (Atom "defsubst" ::
            Atom name ::
            Atom syntax :: body) when is_keyarg syntax ->
      defsubst ~syntax name body state s
    | List (Atom "defsubst" ::
            Atom name ::
            List (Atom "declare" :: attrs) ::
            body) ->
      defsubst ~attrs name body state s
    | List (Atom "defsubst" :: Atom name :: body) ->
      defsubst name body state s
    | List [Atom "advice-add"; Atom f1; h; Atom f2] ->
      add_advice ~advised:f2 ~advisor:f1 (hook h) state s
    | List (Atom "declare" :: attrs) -> {
        state with global_attrs =
                     parse_declarations state.global_attrs attrs
      }
    | List [Atom "require"; Atom name] ->
      state.parse_module name state
    | s -> parse_error Bad_toplevel s


  let defs state = List.fold ~f:stmt ~init:state
end


module Load = struct
  let file_of_feature paths feature =
    let name = feature ^ ".lisp" in
    List.find_map paths ~f:(fun path ->
        Sys.readdir path |> Array.find_map ~f:(fun file ->
            if String.(file = name)
            then Some (Filename.concat path file)
            else None))


  let find_pos pos sexps sub =
    Parsexp.Positions.find_sub_sexp_in_list_phys pos sexps ~sub

  let load_sexps name =
    In_channel.read_all name |>
    Parsexp.Many_and_positions.parse_string |> function
    | Result.Error err -> failwith "TODO"
    | Ok (sexps,pos) -> sexps,pos

  (* TODO: check for cycles *)
  let feature
      ?(features=String.Set.empty)
      ?(paths=[Filename.current_dir_name])
      program cs feature =
    let open Parse.State in
    let rec load (p : program) feature =
      if Set.mem p.modules feature then p
      else match file_of_feature paths feature with
        | Some name ->
          let sexps,pos = load_sexps name in
          let getpos sexp = match find_pos pos sexps sexp with
            | None -> failwith "internal error"
            | Some range -> {
                def = {
                    file=name;
                    range;
                  };
                src = Ground;
              } in
          let modules = Set.add p.modules feature in
          let state = Parse.State.{
              getpos;
              global_attrs = Univ_map.empty;
              program = {p with modules};
              constraints=cs;
              parse_module = (fun feature s ->
                  {s with program = load s.program feature})
            } in
          let result = Parse.defs state sexps in
          result.program
        | None ->
          invalid_argf "Lisp loader: can't find module %s"
            feature () in
    load program feature

end
