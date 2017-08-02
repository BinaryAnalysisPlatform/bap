open Core_kernel.Std
open Bap.Std
open Format
open Bap_c.Std
open Bap_primus_types

type bop = Add | Sub | Mul | Div | Mod | Divs | Mods
         | Lsl | Lsr | Asr | And | Or | Xor | Cat
         | Eq | Le
[@@deriving sexp]
type uop = Neg | Not [@@deriving sexp]

type typ = Word | Type of int [@@deriving sexp]
type 'a scalar = {data : 'a; typ : typ}[@@deriving sexp]
type word = int64 scalar [@@deriving sexp]
type var = string scalar[@@deriving sexp]
type exp =
  | Int of word
  | Var of var
  | Ite of exp * exp * exp
  | Let of var * exp * exp
  | Ext of exp * exp * exp
  | Bop of bop * exp * exp
  | Uop of uop * exp
  | App of string * exp list
  | Seq of exp list
  | Set of var * exp
  | Rep of exp * exp
  | Msg of fmt list * exp list
  | Err of string
and fmt = Lit of string | Exp of exp | Pos of int


type hook = [ `before | `after] [@@deriving sexp]
type advices = (hook * string) list String.Map.t
type attrs = Univ_map.t

type filepos = {
  file  : string;
  range : Sexp.Annotated.range;
}

type loc = Primitive | Filepos of filepos

type meta = {
  name : string;
  docs : string;
  attrs : attrs;
  loc : loc;
}

type func = {
  args : var list;
  body : exp list;
}

type macro = {
  param : string list;
  subst : Sexp.t;
  const : bool;
}

type subst = {
  elts : Sexp.t list;
}

type 'a def = {meta : meta; code : 'a}
type 'a defs = 'a def list

(* for external usage *)

type library = {
  mutable paths : string list;
  mutable features : string list;
  mutable log : formatter;
  mutable initialized : bool;
}

type program = {
  modules : String.Set.t;
  defs : func defs;
  macros : macro defs;
  substs : subst defs;
  advices : advices;
}

let library = {
  paths = [];
  features = [];
  log = err_formatter;
  initialized = false;
}

module Primitive = struct
  type 'a t = (value list -> 'a) def
  let create ?(docs="") name code : 'a t =
    {meta = {name;docs; attrs=Univ_map.empty; loc=Primitive}; code}
end

module type Primitives = functor (Machine : Machine) ->  sig

  (** each primitive is an OCaml function that takes a list of words
      and returns a word. A primitive linkage is internal to the Lisp
      machine, so it is visible only to Lisp functions.
      If you want to implement a stub function in OCaml, then you
      should work directly with the Linker module. The primitives
      extend only the Lisp machine.
  *)
  val defs : unit -> value Machine.t Primitive.t list
end

type primitives = (module Primitives)


let expect what got =
  invalid_argf "Parser error: expected %s, got %s"
    what got ()

let expects what got = expect what (Sexp.to_string got)

module Attrs = struct
  type t = attrs

  type 'a attr = {
    key : 'a Univ_map.Key.t;
    add : 'a -> 'a -> 'a;
    parse : Sexp.t list -> 'a;
  }

  type parser = Parser of (t -> Sexp.t list -> t)

  let parsers : parser String.Table.t = String.Table.create ()

  let make_parser attr attrs sexp =
    let value = attr.parse sexp in
    Univ_map.update attrs attr.key ~f:(function
        | None -> value
        | Some value' -> attr.add value value')

  let register ~name ~add ~sexp_of ~of_sexp =
    let attr = {
      key = Univ_map.Key.create ~name sexp_of;
      add;
      parse = of_sexp;
    } in
    let parser = Parser (make_parser attr) in
    Hashtbl.add_exn parsers ~key:name ~data:parser;
    attr.key

  let expected_parsers () =
    String.Table.keys parsers |> String.concat ~sep:" | "

  let parse attrs name values = match Hashtbl.find parsers name with
    | None -> expect (expected_parsers ()) name
    | Some (Parser run) -> run attrs values

  let parse attrs = function
    | Sexp.List (Sexp.Atom name :: values) -> parse attrs name values
    | s -> expects "(name value ...)" s
end

module Contexts = struct
  module Feature = String
  module Name = String

  type t = Feature.Set.t Name.Map.t
  let empty = Name.Map.empty

  let attr proj attr = match Project.get proj attr with
    | Some x -> [x]
    | None -> []

  let endian proj = Project.arch proj |>
                    Arch.endian |> function
                    | LittleEndian -> "little"
                    | BigEndian -> "big"
  let features = Feature.Set.of_list

  let of_project proj = Name.Map.of_alist_exn [
      "arch", features @@ [
        Arch.to_string (Project.arch proj);
      ] @ attr proj Bap_abi.name;
      "abi", features @@ attr proj Bap_abi.name;
      "endian", features [endian proj]
    ]

  let sexp_of_context (name,values) =
    Sexp.List (List.map (name :: Set.to_list values)
                 ~f:(fun x -> Sexp.Atom x))

  let sexp_of (cs : t) =
    Sexp.List (Sexp.Atom "context" ::
               (Map.to_alist cs |> List.map ~f:sexp_of_context))

  let value = function
    | Sexp.Atom x -> x
    | s -> expects "(a1 a2 ... am)" s

  let context_of_sexp = function
    | Sexp.List (Sexp.Atom name :: values) ->
      name, Feature.Set.of_list (List.map values ~f:value)
    | s -> expects "(a1 a1 ... am)" s


  let push cs name vs =
    Map.update cs name ~f:(function
        | None -> vs
        | Some vs' -> Set.union vs vs')


  let of_sexp : Sexp.t list -> t =
    List.fold ~init:Name.Map.empty ~f:(fun cs sexp ->
        let (name,vs) = context_of_sexp sexp in
        push cs name vs)

  let add cs cs' =
    Map.fold cs ~init:cs' ~f:(fun ~key:name ~data:vs cs' ->
        push cs' name vs)

  let t = Attrs.register
      ~name:"context"
      ~add:add
      ~sexp_of ~of_sexp

  let (<=) = Set.subset
  let pp ppf ctxt =
    Sexp.pp_hum ppf (sexp_of ctxt)
end

module External = struct
  type t = string list

  let sexp_of x = Sexp.List (List.map x ~f:(fun s -> Sexp.Atom s))
  let of_sexp = List.map ~f:(function
      | Sexp.Atom x -> x
      | s -> expects "(external name1 .. nameM)" s)

  let t = Attrs.register
      ~name:"external"
      ~add:List.append
      ~sexp_of ~of_sexp

end

module Macro = struct
  open Sexp

  let take_rest xs ys =
    let rec take xs ys zs = match xs,ys with
      | [],[] -> Some zs
      | [x], (_ :: _ :: ys as rest) -> Some ((x,rest)::zs)
      | x :: xs, y :: ys -> take xs ys ((x,[y])::zs)
      | _ :: _, [] | [],_ -> None in
    match take xs ys []with
    | Some [] -> Some (0,[])
    | Some ((z,rest) :: _ as bs) ->
      Some (List.length rest, List.rev bs)
    | None -> None

  let bind macro cs = take_rest macro.code.param cs

  let subst bs body =
    let rec sub = function
      | List xs -> [List (List.concat_map xs ~f:sub)]
      | Atom x -> match List.Assoc.find bs x with
        | None -> [Atom x]
        | Some cs -> cs in
    match body with
    | List xs -> List (List.concat_map xs ~f:sub)
    | Atom x -> match List.Assoc.find bs x with
      | None -> Atom x
      | Some [x] -> x
      | Some xs -> invalid_argf "invalid substitution" ()

  let apply macro cs =
    subst cs macro.code.subst
end

module Resolve = struct
  type stage = loc list
  type resolution = {
    stage1 : stage; (* definitions with the given name *)
    stage2 : stage; (* definitions applicable to the ctxt *)
    stage3 : stage; (* most specific definitions *)
    stage4 : stage; (* applicable definitions *)
  }

  type 'a candidate = 'a def * Contexts.t
  type 'a candidates = 'a candidate list

  type exn += Failed of string * Contexts.t * resolution

  let pp_loc ppf loc = match loc with
    | Primitive -> fprintf ppf "<primitive>"
    | Filepos {file; range={Sexp.Annotated.start_pos = {
        Sexp.Annotated.line;
      }}} ->
      fprintf ppf "File %s, line %d" file line

  let pp_stage ppf locs =
    List.iter locs ~f:(fun loc ->
        fprintf ppf "%a@\n" pp_loc loc)

  let pp ppf {stage1; stage2; stage3; stage4} =
    fprintf ppf "Initial set of candidates: @\n%a@\n\
                 Candidates that satisfy current context: @\n%a@\n\
                 Most specific candidates: @\n%a@\n\
                 Candidates with compatible types and arity: @\n%a@\n\ "
      pp_stage stage1
      pp_stage stage2
      pp_stage stage3
      pp_stage stage4

  let string_of_error name ctxts resolution =
    asprintf
      "no candidate for definition %s@\n\
       evaluation context@\n%a@\n@\n%a"
      name Contexts.pp ctxts pp resolution

  let () = Exn.add_printer (function
      | Failed (n,c,r) -> Some (string_of_error n c r)
      | _ -> None)


  let interns def name = def.meta.name = name
  let externs def name =
    match Univ_map.find def.meta.attrs External.t with
    | None -> false
    | Some names -> List.mem names name


  (* all definitions with the given name *)
  let stage1 has_name defs name : 'a candidates =
    List.filter_map defs ~f:(fun def ->
        if has_name def name
        then match Univ_map.find def.meta.attrs Contexts.t with
          | None -> Some (def,Contexts.empty)
          | Some ctxts -> Some (def,ctxts)
        else None)


  (* all definitions that satisfy the [ctxts] constraint *)
  let stage2 (ctxts : Contexts.t) : 'a candidates -> 'a candidates =
    List.filter ~f:(fun (def,ctxts') ->
        Map.for_alli ctxts' ~f:(fun ~key:name' ~data:ctxt' ->
            Map.existsi ctxts ~f:(fun ~key:name ~data:ctxt ->
                name' = name && Contexts.(ctxt' <= ctxt))))

  (* remove all definitions that has a class that is a supertype
     (i.e., less specific) of the same class of some other
     definition.

     This is a final step in the context resolution, if there are more
     than one candidates left, then the context is ambigious, as we
     have more than one candidate that is applicable to it.*)

  let stage3 (s2 : 'a candidates) : 'a candidates =
    List.filter s2 ~f:(fun (_,ctxts') ->
        Map.for_alli ctxts' ~f:(fun ~key:cls' ~data:ctxt' ->
            List.for_all s2 ~f:(fun (_,ctxts) ->
                Map.for_alli ctxts ~f:(fun ~key:cls ~data:ctxt ->
                    String.(cls <> cls') || Contexts.(ctxt <= ctxt')))))

  (* XXX: looks like I forgot to apply SFINAE to the resolution *)
  let typechecks arch (v,w) =
    let word_size = Size.in_bits (Arch.addr_size arch) in
    let size = Word.bitwidth w in
    match v.typ with
    | Word -> size = word_size
    | Type n -> size = n

  let overload_macro code (s3 : 'a candidates) =
    List.filter_map s3 ~f:(fun (def,_) ->
        Option.(Macro.bind def code >>| fun (n,bs) -> n,def,bs)) |>
    List.sort ~cmp:(fun (n,_,_) (m,_,_) -> Int.ascending n m) |> function
    | [] -> []
    | ((n,_,_) as c) :: cs -> List.filter_map (c::cs) ~f:(fun (m,d,bs) ->
        Option.some_if (n = m) (d,bs))

  let overload_defun arch args s3 =
    List.filter_map s3 ~f:(fun (def,cs) ->
        Option.(List.zip def.code.args args >>| fun bs -> def,bs))

  let overload_primitive s3 = s3

  let locs = List.map ~f:(fun (def,_) -> def.meta.loc)

  let run namespace overload ctxts defs name =
    let s1 = stage1 namespace defs name in
    let s2 = stage2 ctxts s1 in
    let s3 = stage3 s2 in
    let s4 = overload s3 in
    let result = match s4 with
      | [f] -> Some f
      | _ -> None in
    {
      stage1 = locs s1;
      stage2 = locs s2;
      stage3 = locs s3;
      stage4 = locs s4;
    }, result

  let extern ctxts defs name arch args =
    run externs (overload_defun arch args) ctxts defs name

  let defun ctxts defs name arch args =
    run interns (overload_defun arch args) ctxts defs name

  let macro ctxts defs name code =
    run interns (overload_macro code) ctxts defs name

  let primitive ctxts defs name =
    run interns overload_primitive ctxts defs name
end

module Type_annot = struct
  let parse sz = Type (int_of_string (String.strip sz))
end

module Variable = struct
  type t = var

  let parse x = match String.split x ~on:':' with
    | [x;sz] -> {data=x; typ = Type_annot.parse sz}
    | _ -> {data=x;typ=Word}

  let to_string = function
    | {data;typ = Word} -> data
    | {data;typ = Type n} -> sprintf "%s:%d" data n

  let sexp_of_t v = Sexp.Atom (to_string v)
end

module Variables = struct
  type t = var list

  let var = function
    | Sexp.Atom v -> Variable.parse v
    | s -> expects "(v1 .. vm)" s

  let of_sexp = List.map ~f:var
  let sexp_of vs = Sexp.List (List.map vs ~f:Variable.sexp_of_t)

  let global = Attrs.register
      ~name:"global"
      ~add:List.append
      ~sexp_of ~of_sexp

  let static = Attrs.register
      ~name:"static"
      ~add:List.append
      ~sexp_of ~of_sexp
end

module Parse = struct
  module State = struct
    type t = {
      global_attrs : attrs;
      parse_module : string -> t -> t;
      program : program;
      current : filepos;
      constraints : Contexts.t;
    }
  end
  open Sexp

  let atom = function
    | Atom x -> x
    | s -> expects "atom" s

  let typ = Type_annot.parse

  let char x = Int {
      data = Int64.of_int (Char.to_int (Char.of_string x));
      typ = Type 8
    }

  let word x =
    if Char.(x.[0] = '?') then char (String.subo ~pos:1 x)
    else match String.split x ~on:':' with
      | [x] ->  Int {data=Int64.of_string x; typ=Word}
      | [x;sz] -> Int {
          data = Int64.of_string (String.strip x);
          typ   = typ sz
        }
      | _ -> expect "int ::= ?<char> | <lit> | <lit>:<typ>" x


  let is_word x = try ignore (word x);true with _ -> false
  let var = Variable.parse
  let int x = int_of_string (atom x)

  let bop op = match atom op with
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | "%" -> Mod
    | "%s" -> Mods
    | "/s" -> Divs
    | "<<" -> Lsl
    | ">>" -> Lsr
    | ">>s" -> Asr
    | "=" -> Eq
    | "<" -> Le
    | "&" -> And
    | "|" -> Or
    | _ -> bop_of_sexp op

  let is_bop x = try ignore (bop x); true with exn -> false

  let bop x e1 e2 = Bop (bop x, e1, e2)


  let bad_form pos op got =
    let open Sexp.Annotated in
    invalid_argf {|\nFile "%s", line %d \
                   parser error: invalid usage of form %s. \
                   Don't know how to handle\n%s|}
      pos.file pos.range.start_pos.line op (Sexp.to_string_hum got) ()


  let keywords = [
    "if"; "let"; "neg"; "not";
    "coerce"; "msg"; "while";"set"
  ]
  let is_keyword op = List.mem keywords op
  let nil = Int {data=0L; typ=Word}

  let find_entry defs op =
    List.find defs ~f:(fun m -> String.(m.meta.name = op))

  let find_macro {State.program={macros}} = find_entry macros
  let find_subst {State.program={substs}} = find_entry substs

  let is_macro s op = Option.is_some (find_macro s op)

  let is_const s op = match find_macro s op with
    | None -> false
    | Some {code={const}} -> const


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

  let subst {State.constraints=cs; program={macros}} name ops =
    match Resolve.macro cs macros name ops with
    | res,None -> failwith (Resolve.string_of_error name cs res)
    | _,Some (macro,bs) -> Macro.apply macro bs

  let exp ({State.current=pos; constraints=ctxts} as s) sexp =
    let rec exp = function
      | Atom x when is_word x -> word x
      | Atom c when is_const s c -> exp (subst s c [])
      | Atom x -> Var (var x)
      | List (Atom "if" :: c :: e1 :: es) -> Ite (exp c,exp e1,prog es)
      | List (Atom "let" :: List bs :: e) -> let' bs e
      | List [Atom "coerce"; e1; e2; e3] -> Ext (exp e1,exp e2,exp e3)
      | List [Atom "neg"; e] -> Uop (Neg, exp e)
      | List [Atom "not"; e] -> Uop (Not, exp e)
      | List (Atom "prog" :: es) -> prog es
      | List (Atom "while" :: c :: es) -> Rep (exp c, prog es)
      | List [Atom "set"; Atom x; e] -> Set (var x, exp e)
      | List (Atom "msg" :: Atom msg :: es) -> Msg (fmt msg, exps es)
      | List [Atom "fail"; Atom msg] -> Err msg
      | List (Atom op :: _) as exp when is_keyword op -> bad_form pos op exp
      | List (Atom op :: exps) when is_macro s op ->
        exp (subst s op (expand s exps))
      | List (op :: arg :: args) when is_bop op -> expbop op arg args
      | List (Atom op :: args) -> App (op, exps (expand s args))
      | List [] -> nil
      | s ->  expects "(<ident> exps..)" s
    and exps = List.map ~f:exp
    and prog es = Seq (exps es)
    and expbop op arg args =
      match expand s (arg::args) with
      | arg :: args ->
        List.fold ~f:(bop op) ~init:(exp arg) (exps args)
      | [] -> expect "(<bop> exps...)" "(<bop>)"
    and let' bs e =
      List.fold_right bs ~init:(prog e) ~f:(fun b e ->
          match b with
          | List [Atom v; x] -> Let (var v,exp x,e)
          | s -> expects "(var exp)" s)
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
    | s -> expects "(v1 v2 ..)" s

  let metaparams = function
    | List vars -> List.map ~f:atom vars
    | s -> expects "(s1 s2 ..)" s

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

  let defun ?(docs="") ?(attrs=[]) name p body state = {
    state with
    program = add_def state.program {
        meta = {
          name;
          docs;
          attrs = parse_declarations state.global_attrs attrs;
          loc = Filepos state.current;
        };
        code = {
          args = params p;
          body = List.map ~f:(exp state) body
        }
      }
  }

  let defmacro ?(docs="") ?(attrs=[]) ?(const=false)
      name ps body state = {
    state with
    program = add_macro state.program {
        meta = {
          name;
          docs;
          attrs = parse_declarations state.global_attrs attrs;
          loc = Filepos state.current;
        };
        code = {
          param = metaparams ps;
          subst = body;
          const;
        }
      }
  }

  let defsubst ?(attrs=[]) ?syntax name body state = {
    state with
    program = add_subst state.program {
        meta = {
          name;
          docs="";
          attrs = parse_declarations state.global_attrs attrs;
          loc = Filepos state.current;
        };
        code = {
          elts = reader syntax body
        }
      }
  }

  let add_advice ~advisor ~advised where state = {
    state with
    program = {
      state.program with
      advices = String.Map.add_multi
          state.program.advices ~key:advised ~data:(where,advisor)
    }
  }

  let hook = function
    | ":before" -> `before
    | ":after" -> `after
    | s -> expects ":before | :after" (Atom s)


  let stmt state = function
    | List (Atom "defun" ::
            Atom name ::
            params ::
            Atom docs ::
            List (Atom "declare" :: attrs) ::
            body) ->
      defun ~docs ~attrs name params body state
    | List (Atom "defun" ::
            Atom name ::
            params ::
            List (Atom "declare" :: attrs) ::
            body) ->
      defun ~attrs name params body state
    | List (Atom "defun" ::
            Atom name ::
            params ::
            Atom const :: []) ->
      defun name params [Atom const] state
    | List (Atom "defun" ::
            Atom name ::
            params ::
            Atom docs ::
            body) ->
      defun ~docs name params body state
    | List (Atom "defun" ::
            Atom name ::
            params ::
            body) ->
      defun name params body state
    | List [Atom "defconstant";
            Atom name;
            Atom docs;
            List (Atom "declare" :: attrs);
            Atom _ as value;
           ] ->
      defmacro ~docs ~attrs ~const:true name (List []) value state
    | List [Atom "defconstant";
            Atom name;
            List (Atom "declare" :: attrs);
            Atom _ as value;
           ] ->
      defmacro ~attrs ~const:true name (List []) value state
    | List [Atom "defconstant";
            Atom name;
            Atom docs;
            Atom _ as value;
           ] ->
      defmacro ~docs ~const:true name (List []) value state
    | List [Atom "defconstant";
            Atom name;
            Atom _ as value;
           ] ->
      defmacro ~const:true name (List []) value state
    | List [Atom "defmacro";
            Atom name;
            params;
            Atom docs;
            List (Atom "declare" :: attrs);
            body] ->
      defmacro ~docs ~attrs name params body state
    | List [Atom "defmacro";
            Atom name;
            params;
            List (Atom "declare" :: attrs);
            body] ->
      defmacro ~attrs name params body state
    | List [Atom "defmacro";
            Atom name;
            params;
            Atom docs;
            body] ->
      defmacro ~docs name params body state
    | List [Atom "defmacro";
            Atom name;
            params;
            body] ->
      defmacro name params body state
    | List (Atom "defsubst" ::
            Atom name ::
            List (Atom "declare" :: attrs) ::
            Atom syntax :: body) when is_keyarg syntax ->
      defsubst ~attrs ~syntax name body state
    | List (Atom "defsubst" ::
            Atom name ::
            Atom syntax :: body) when is_keyarg syntax ->
      defsubst ~syntax name body state
    | List (Atom "defsubst" ::
            Atom name ::
            List (Atom "declare" :: attrs) ::
            body) ->
      defsubst ~attrs name body state
    | List (Atom "defsubst" :: Atom name :: body) ->
      defsubst name body state
    | List [Atom "advice-add"; Atom f1; Atom h; Atom f2] ->
      add_advice ~advised:f2 ~advisor:f1 (hook h) state
    | List (Atom "declare" :: attrs) -> {
        state with global_attrs =
                     parse_declarations state.global_attrs attrs
      }
    | List [Atom "require"; Atom name] ->
      state.parse_module name state
    | s -> expects "(defun ...) | (defmacro ..) | (defconstant ..) \
                    (declare ..) | (require ..) | (advice-add .. )" s


  let update_range loc range = Sexp.Annotated.{
      loc with range
    }

  let update_file loc name = Sexp.Annotated.{
      loc with name
    }

  let annotated_stmt state sexp =
    let range = Sexp.Annotated.get_range sexp in
    let sexp = Sexp.Annotated.get_sexp sexp in
    stmt {state with current = update_range state.current range} sexp

  let defs state = List.fold ~f:annotated_stmt ~init:state
end

module Load = struct

  let make_pos ~line ~col ~offset = Sexp.Annotated.{
      line; col; offset;
    }

  let dummy_pos = make_pos ~line:0 ~col:0 ~offset:0

  let make_range ~start_pos ~end_pos = Sexp.Annotated.{
      start_pos; end_pos
    }

  let dummy_range = make_range ~start_pos:dummy_pos ~end_pos:dummy_pos

  let make_loc range name = Sexp.Annotated.{
      file = name; range;
    }

  let load_sexps = Sexp.Annotated.load_sexps

  let file_of_feature paths feature =
    let name = feature ^ ".lisp" in
    List.find_map paths ~f:(fun path ->
        Sys.readdir path |> Array.find_map ~f:(fun file ->
            if String.(file = name)
            then Some (Filename.concat path file)
            else None))

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
          let modules = Set.add p.modules feature in
          let state = Parse.State.{
              current = make_loc dummy_range name;
              global_attrs = Univ_map.empty;
              program = {p with modules};
              constraints=cs;
              parse_module = (fun feature s ->
                  {s with program = load s.program feature})
            } in
          let result = Parse.defs state (load_sexps name) in
          result.program
        | None ->
          invalid_argf "Lisp loader: can't find module %s"
            feature () in
    load program feature

end


module State = Bap_primus_state

type bindings = (var * value) list [@@deriving sexp]

type state = {
  primitives : primitives list;
  program : program;
  width : int;
  env : bindings;
  contexts : Contexts.t;
  paths : string list;
}


let inspect_def {meta={name; docs}} = Sexp.List [
    Sexp.Atom name;
    Sexp.Atom docs;
  ]

let inspect {env} = sexp_of_bindings env

let width_of_ctxt proj =
  Size.in_bits (Arch.addr_size (Project.arch proj))


type exn += Runtime_error of string
type exn += Link_error of string

let () = Exn.add_printer (function
    | Runtime_error msg -> Some ("primus runtime error - " ^ msg)
    | Link_error msg -> Some ("primus linker error - " ^ msg)
    | _ -> None)

let empty_program = {
  modules = String.Set.empty;
  advices = String.Map.empty;
  defs    = [];
  macros  = [];
  substs  = [];
}

let state = Bap_primus_state.declare ~inspect
    ~name:"lisp-env"
    ~uuid:"fc4b3719-f32c-4d0f-ad63-6167ab00b7f9"
    (fun proj -> {
         env = [];
         primitives = [];
         program = empty_program;
         paths = [Filename.current_dir_name];
         width = width_of_ctxt proj;
         contexts = Contexts.of_project proj;
       })

module Trace = struct
  module Observation = Bap_primus_observation
  let sexp_of_value {value=x} =
    let v = Word.string_of_value x in
    let w = Int.to_string (Word.bitwidth x) in
    Sexp.Atom (v ^ ":" ^ w)
  let sexp_of_binding (_,x) = sexp_of_value x

  let sexp_of_enter ({meta={name}},bs) =
    Sexp.List (Sexp.Atom name :: List.map bs ~f:sexp_of_binding)

  let sexp_of_leave (call,result) =
    Sexp.List (Sexp.Atom "#result-of" ::
               sexp_of_enter call ::
               [sexp_of_value result])

  let enter,entered =
    Observation.provide ~inspect:sexp_of_enter "lisp-call"

  let leave,left =
    Observation.provide ~inspect:sexp_of_leave "lisp-return"

end

module Locals(Machine : Machine) = struct
  open Machine.Syntax

  let rec update xs x ~f = match xs with
    | [] -> []
    | (x',w) :: xs when x' = x -> (x,f w) :: xs
    | xw :: xs -> xw :: update xs x ~f

  let replace xs x w = update xs x ~f:(fun _ -> w)

  let push v w s = {s with env = (v,w)::s.env}
  let pop n ({env} as s) = {s with env = List.drop env n}
end

module Lisp(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Vars = Locals(Machine)
  module Env = Bap_primus_env.Make(Machine)
  module Mem = Bap_primus_memory.Make(Machine)
  module Value = Bap_primus_value.Make(Machine)

  let error kind = Format.ksprintf
      (fun msg -> fun () -> Machine.raise (Runtime_error msg))

  let failf fmt = error (fun m -> Runtime_error m) fmt
  let linkerf fmt = error (fun m -> Link_error m) fmt

  let word width value typ =
    let width = match typ with
      | Word -> width
      | Type n -> n in
    Word.of_int64 ~width value

  let var width {data;typ} =
    let typ = match typ with
      | Word -> Type.Imm width
      | Type n -> Type.Imm n in
    Var.create data typ

  let width () = Machine.Local.get state >>| fun {width} -> width


  let stack_slot exp =
    let open Bil.Types in match exp with
    | BinOp (PLUS, Var sp, Int off) -> Some (sp,`down,off)
    | BinOp (MINUS,Var sp, Int off) -> Some (sp,`up,off)
    | _ -> None

  let update_frame slot addr n =
    match slot, stack_slot addr with
    | slot,None -> slot
    | None, Some (sp,dir,off) ->
      Some (sp,dir,Word.(off ++ Size.in_bytes n))
    | Some (sp,dir,off), Some (sp',dir',off') ->
      if Var.same sp sp' && dir = dir' then
        let off = Word.max off off' in
        Some (sp,dir,Word.(off ++ Size.in_bytes n))
      else Some (sp,dir,off)


  let find_max_slot =
    Seq.fold ~init:None ~f:(fun slot arg ->
        match Arg.rhs arg with
        | Bil.Load (_,addr,_,n) -> update_frame slot addr n
        | _ -> slot)

  let allocate_stack_frame args =
    match find_max_slot args with
    | None -> Machine.return None
    | Some (sp,dir,max) ->
      let sign = if dir = `down then Bil.MINUS else Bil.PLUS in
      Eval.get sp >>= fun sp_value ->
      Eval.const max >>= fun frame_size ->
      Eval.binop sign sp_value frame_size >>=
      Eval.set sp >>= fun () ->
      Machine.return (Some (sp,sp_value))


  let bil_of_lisp op =
    let open Bil in
    let binop = Eval.binop in
    match op with
    | Add  -> binop plus
    | Sub  -> binop minus
    | Mul  -> binop times
    | Div  -> binop divide
    | Mod  -> binop modulo
    | Divs -> binop sdivide
    | Mods -> binop smodulo
    | Lsl  -> binop lshift
    | Lsr  -> binop rshift
    | Asr  -> binop arshift
    | And  -> binop AND
    | Or   -> binop OR
    | Xor  -> binop XOR
    | Cat  -> Eval.concat
    | Eq   -> binop eq
    | Le   -> binop le


  let eval_sub : value list -> 'x = function
    | [] -> failf "invoke-subroutine: requires at least one argument" ()
    | sub_addr :: sub_args ->
      Machine.get () >>= fun proj ->
      Term.enum sub_t (Project.program proj) |>
      Seq.find ~f:(fun sub -> match Term.get_attr sub address with
          | None -> false
          | Some addr -> Word.(addr = sub_addr.value)) |> function
      | None ->
        failf "invoke-subroutine: no function for %a" Addr.pps
          sub_addr.value ()
      | Some sub ->
        let args = Term.enum arg_t sub in
        allocate_stack_frame args >>= fun frame ->
        Seq.zip args (Seq.of_list sub_args) |>
        Machine.Seq.iter ~f:(fun (arg,x) ->
            let open Bil.Types in
            if Arg.intent arg <> Some Out
            then match Arg.rhs arg with
              | Var v -> Eval.set v x
              | Load (_,BinOp (op, Var sp, Int off),endian,size) ->
                Eval.get sp  >>= fun sp ->
                Eval.const off >>= fun off ->
                Eval.binop op sp off >>= fun addr ->
                Eval.store addr x endian size
              | exp ->
                failf "%s: can't pass argument %s - %s %a"
                  "invoke-subroutine" (Arg.lhs arg |> Var.name)
                  "unsupported ABI" Exp.pps exp ()
            else Machine.return ()) >>= fun () ->
        Linker.exec (`addr sub_addr.value) >>= fun () ->
        Machine.Seq.find_map args ~f:(fun arg ->
            if Arg.intent arg = Some Out
            then Eval.get (Arg.lhs arg) >>| Option.some
            else Machine.return None) >>= fun rval ->
        let teardown_frame = match frame with
          | Some (sp,bp) -> Eval.set sp bp
          | None -> Machine.return () in
        teardown_frame >>= fun () -> match rval with
        | None -> Eval.const Word.b0
        | Some rval -> Machine.return rval

  let rec eval_lisp name args : value Machine.t =
    Machine.get () >>= fun proj ->
    let arch = Project.arch proj in
    Machine.Local.get state >>= fun s ->
    match Resolve.defun s.contexts s.program.defs name arch args with
    | {Resolve.stage1=[]},None -> eval_primitive name args
    | resolution,None ->
      Machine.raise (Resolve.Failed (name, s.contexts, resolution))
    | _,Some (fn,bs) ->
      Eval.const Word.b0 >>= fun init ->
      eval_advices `before init name args >>= fun _ ->
      Machine.Local.put state {s with env = bs @ s.env} >>= fun () ->
      Machine.Observation.make Trace.entered (fn,bs) >>= fun () ->
      eval_body fn.code.body >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
      Machine.Observation.make Trace.left ((fn,bs),r) >>= fun () ->
      eval_advices `after r name args

  and eval_advices stage init primary args =
    Machine.Local.get state >>= fun {program={advices}} ->
    match Map.find advices primary with
    | Some advices ->
      Machine.List.fold advices ~init ~f:(fun r (s,name) ->
          if s = stage
          then eval_lisp name args
          else Machine.return r)
    | None -> Machine.return init

  and eval_primitive name args =
    Machine.Local.get state >>= fun {contexts; primitives} ->
    let defs = List.concat_map primitives (fun (module Make) ->
        let module Primitives = Make(Machine) in
        Primitives.defs ()) in
    match Resolve.primitive contexts defs name with
    | _,None -> failf "unresolved name %s" name ()
    | _,Some (def,_) ->
      Eval.const Word.b0 >>= fun init ->
      eval_advices `before init name args >>= fun _ ->
      def.code args >>= fun r ->
      eval_advices `after r name args

  and eval_body body : value Machine.t = eval_exp (Seq body)

  and eval_exp exp  =
    let int v t = width () >>= fun width ->
      Eval.const (word width v t) in
    let rec eval = function
      | Int {data;typ} -> int data typ
      | Var v -> lookup v
      | Ite (c,e1,e2) -> ite c e1 e2
      | Let (v,e1,e2) -> let_ v e1 e2
      | Ext (hi,lo,e) -> ext hi lo e
      | App (n,args) -> app n args
      | Rep (c,e) -> rep c e
      | Bop (op,e1,e2) -> bop op e1 e2
      | Uop (op,e) -> uop op e
      | Seq es -> seq es
      | Set (v,e) -> eval e >>= set v
      | Msg (fmt,es) -> msg fmt es
      | Err msg -> Machine.raise (Runtime_error msg)
    and rep c e =
      eval c >>= function {value} as r ->
        if Word.is_zero value then Machine.return r
      else eval e >>= fun _ -> rep c e
    and ite c e1 e2 =
      eval c >>= fun {value=w} ->
      if Word.is_zero w then eval e2 else eval e1
    and let_ v e1 e2 =
      eval e1 >>= fun w ->
      Machine.Local.update state ~f:(Vars.push v w) >>=  fun () ->
      eval e2 >>= fun r ->
      Machine.Local.update state ~f:(Vars.pop 1) >>= fun () ->
      Machine.return r
    and ext hi lo w =
      let eval_to_int e =
        eval e >>= fun {value=x} -> match Word.to_int x with
        | Ok x -> Machine.return x
        | Error _ -> failf "expected smallint" () in
      eval_to_int hi >>= fun hi ->
      eval_to_int lo >>= fun lo ->
      eval w >>= fun w ->
      Eval.extract ~hi ~lo w
    and lookup v =
      Machine.Local.get state >>= fun {env; width} ->
      match List.Assoc.find env v with
      | Some w -> Machine.return w
      | None -> Eval.get (var width v)
    and app n args =
      Machine.List.map args ~f:eval >>= fun args -> match n with
      | "invoke-subroutine" -> eval_sub args
      | n -> eval_lisp n args
    and seq es =
      let rec loop = function
        | [] -> Eval.const Word.b0
        | e :: [] -> eval e
        | e :: es -> eval e >>= fun _ -> loop es in
      loop es
    and set v w =
      Machine.Local.get state >>= fun s ->
      if List.Assoc.mem s.env v
      then
        Machine.Local.put state {s with env = Vars.replace s.env v w}
        >>= fun () -> Machine.return w
      else
        Eval.set (var s.width v) w >>= fun () ->
        Machine.return w
    and bop op e1 e2 =
      eval e1 >>= fun e1 ->
      eval e2 >>= fun e2 ->
      bil_of_lisp op e1 e2
    and uop op e =
      eval e >>= fun e ->
      let op = match op with
        | Neg -> Bil.NEG
        | Not -> Bil.NOT in
      Eval.unop op e
    and msg fmt es =
      let pp_exp e =
        Machine.catch
          (eval e >>| fun {value=x} -> fprintf library.log "%a" Word.pp x)
          (fun err ->
             fprintf library.log "<%s>" (Exn.to_string err);
             Machine.return ()) in
      Machine.List.iter fmt ~f:(function
          | Lit s -> Machine.return (pp_print_string library.log s)
          | Exp e -> pp_exp e
          | Pos n -> match List.nth es n with
            | None -> Machine.raise (Runtime_error "fmt pos")
            | Some e -> pp_exp e) >>= fun () ->
      pp_print_newline library.log ();
      Eval.const Word.b0 in
    eval exp

end

module Make(Machine : Machine) = struct
  open Machine.Syntax
  module Linker = Bap_primus_linker.Make(Machine)
  module Eval = Bap_primus_interpreter.Make(Machine)
  module Vars = Locals(Machine)
  module Value = Bap_primus_value.Make(Machine)


  let error kind = Format.ksprintf
      (fun msg -> fun () -> Machine.raise (kind msg))

  let failf fmt = error (fun m -> Runtime_error m) fmt
  let linkerf fmt = error (fun m -> Link_error m) fmt

  let collect_externals s =
    List.fold ~init:String.Map.empty s.program.defs ~f:(fun toload def ->
        match Univ_map.find def.meta.attrs External.t with
        | Some names ->
          List.fold names ~init:toload ~f:(fun toload name ->
              Map.add_multi toload ~key:name ~data:def)
        | _ -> toload) |>
    Map.to_sequence

  let find_sub prog name =
    Term.enum sub_t prog |>
    Seq.find ~f:(fun s -> Sub.name s = name) |> function
    | None -> [],None,None,None
    | Some sub ->
      let tid = Some (Term.tid sub) in
      let addr = Term.get_attr sub address in
      match Term.get_attr sub C.proto with
      | None -> [],None,tid,addr
      | Some proto ->
        let args = Term.enum arg_t sub |> Seq.to_list in
        let args,ret = match proto.C.Type.Proto.return with
          | `Void -> args,None
          | _ -> List.(take args (List.length args - 1), last args) in
        args,ret,tid,addr

  let link_feature (name,defs) =
    fprintf library.log "linking %s@\n" name;
    Machine.get () >>= fun proj ->
    Machine.Local.get state >>= fun s ->
    let arch = Project.arch proj in
    let args,ret,tid,addr = find_sub (Project.program proj) name in
    match Resolve.extern s.contexts defs name arch args with
    | res,None when tid = None -> Machine.return ()
    | res,None -> Machine.raise (Resolve.Failed (name,s.contexts,res))
    | _,Some (fn,bs) ->
      let module Code(Machine : Machine) = struct
        open Machine.Syntax
        module Eval = Bap_primus_interpreter.Make(Machine)
        module Lisp = Lisp(Machine)

        let failf ppf = Format.ksprintf
            (fun msg -> fun () -> Machine.raise (Runtime_error msg)) ppf

        let eval_args = Machine.List.map bs ~f:(fun (var,arg) ->
            let open Bil.Types in
            match Arg.rhs arg with
            | Var v -> Eval.get v >>| fun w -> (var,w)
            | Load (_,BinOp(op, Var sp, Int off),e,s) ->
              Eval.get sp >>= fun sp ->
              Eval.const off >>= fun off ->
              Eval.binop op sp off >>= fun addr ->
              Eval.load addr e s >>| fun w -> (var,w)
            | _ -> failf "unsupported argument passing sematics" ())

        let eval_ret r = match ret with
          | None -> Machine.return ()
          | Some v -> match Arg.rhs v with
            | Bil.Var reg -> Eval.set reg r
            | e -> failf "unknown return semantics: %a" Exp.pps e ()

        let exec =
          Machine.get () >>= fun ctxt ->
          eval_args >>= fun bs ->
          let args = List.map ~f:snd bs in
          Eval.const Word.b0 >>= fun init ->
          Lisp.eval_advices `before init name args >>= fun _ ->
          Machine.Local.update state
            ~f:(fun s -> {s with env = bs @ s.env}) >>= fun () ->
          Machine.Observation.make Trace.entered (fn,bs) >>= fun () ->
          Lisp.eval_body fn.code.body >>= fun r ->
          Machine.Local.update state ~f:(Vars.pop (List.length bs)) >>= fun () ->
          Machine.Observation.make Trace.left ((fn,bs),r) >>= fun () ->
          Lisp.eval_advices `after r name args >>= fun r ->
          eval_ret r
      end in
      Linker.link ?addr ?tid ~name (module Code)

  let link_features () =
    Machine.Local.get state >>| collect_externals >>=
    Machine.Seq.iter ~f:link_feature

  let load_feature name =
    fprintf library.log "loading feature %s@\n" name;
    Machine.Local.update state ~f:(fun s ->
        let program =
          Load.feature ~paths:s.paths s.program s.contexts name in
        {s with program})

  let load_features fs =
    Machine.List.iter fs ~f:load_feature

  let add_directory path =
    fprintf library.log "adding path %s@\n" path;
    Machine.Local.update state ~f:(fun s -> {
          s with paths = s.paths @ [path]
        })


  let init_env proj = (object
    inherit [(var * value) Machine.t list] Term.visitor
    method! enter_term _ t env =
      match Term.get_attr t address with
      | None -> env
      | Some addr ->
        let binding =
          Value.create addr >>| fun addr ->
          {data = Term.name t; typ = Word}, addr in
        binding :: env
  end)#run proj [] |> Machine.List.all


  let init () =
    fprintf library.log "initializing lisp library@\n";
    Machine.List.iter library.paths ~f:add_directory >>= fun () ->
    load_features library.features >>=
    link_features >>= fun () ->
    fprintf library.log "the lisp machine is ready@\n";
    Machine.return ()

  let link_primitives p =
    Machine.Local.update state ~f:(fun s ->
        {s with primitives = p :: s.primitives})
end

let init ?(log=std_formatter) ?(paths=[]) features  =
  if library.initialized
  then invalid_argf "Lisp library is already initialized" ();
  library.initialized <- true;
  library.paths <- paths;
  library.features <- features;
  library.log <- log;
  Bap_primus_main.add_component (module Make)
