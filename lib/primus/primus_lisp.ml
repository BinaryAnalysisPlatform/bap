open Core_kernel.Std
open Bap.Std
open Primus_types

type bop = Add | Sub | Mul | Div | Mod | Divs | Mods
         | Lsl | Lsr | Asr | And | Or | Xor | Cat
         | Eq | Le
         [@@deriving sexp]
type uop = Neg | Not [@@deriving sexp]

type typ = Word | Type of int [@@deriving sexp]
type 'a scalar = {value : 'a; typ : typ}[@@deriving sexp]
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
  | Seq of exp list * bool option
  | Set of var * exp
  | Rep of exp * exp
  | Err of string


type hook = [`enter | `leave] [@@deriving sexp]
type attrs = Univ_map.t

type filepos = {
  file  : string;
  range : Sexp.Annotated.range;
}

type loc = Builtin | Filepos of filepos

type meta = {
  name : string;
  docs : string;
  attrs : attrs;
  hooks : (hook * string) list;
  loc : loc;
}

type func = {
  args : var list;
  body : exp list;
}

type macro = {
  param : string list;
  subst : Sexp.t;
}

type 'a def = {meta : meta; code : 'a}
type 'a defs = 'a def list

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

  let of_project proj = Name.Map.of_alist_exn [
      "arch",
      Feature.Set.of_list [
        Arch.to_string (Project.arch proj);
      ]
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

  let bind macro cs =
    let rec bind ps cs = match macro.code.param, cs with
      | [],[] -> []
      | [p],cs -> [p,cs]
      | (p::ps),(c::cs) -> (p,[c]) :: bind ps cs
      | _ -> raise Not_found in
    try Some (bind macro.code.param cs) with Not_found -> None

  let rec subst bs : Sexp.t -> Sexp.t = function
    | List xs -> List (List.concat_map xs ~f:(function
        | List xs -> [list bs xs]
        | Atom x -> atom bs x))
    | Atom x -> Atom x
  and list bs xs = List (List.map ~f:(subst bs) xs)
  and atom bs x = match List.Assoc.find bs x with
    | None -> [Atom x]
    | Some cs -> cs


  let apply macro cs = subst cs macro.code.subst
end

module Resolve = struct
  type stage = loc list
  type resolution = {
    stage1 : stage; (* definitions with the given name *)
    stage2 : stage; (* definitions applicable to the ctxt *)
    stage3 : stage; (* most specific definitions *)
    stage4 : stage; (* applicable definitions *)
  }

  type error += Failed of resolution

  (* all definitions with the given name *)
  let stage1 defs name =
    List.filter_map defs ~f:(fun def ->
        if def.meta.name <> name then None
        else match Univ_map.find def.meta.attrs Contexts.t with
          | None -> Some (def,Contexts.empty)
          | Some ctxts -> Some (def,ctxts))

  (* all definitions that satisfy the [ctxts] constraint *)
  let stage2 ctxts =
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
  let stage3 s2  =
    List.filter s2 ~f:(fun (_,ctxts') ->
        Map.for_alli ctxts' ~f:(fun ~key:cls' ~data:ctxt' ->
            List.for_all s2 ~f:(fun (_,ctxts) ->
            Map.for_alli ctxts ~f:(fun ~key:cls ~data:ctxt ->
                cls <> cls' || Context.(ctxt <= ctxt')))))

  let typechecks arch (v,w) =
    let word_size = Size.in_bits (Arch.addr_size arch) in
    let size = Word.bitwidth w in
    match v.typ with
    | Word -> size = word_size
    | Type n -> size = n


  let overload_macro code s3 =
    List.filter_map s3 ~f:(fun (macro,_) ->
        Option.(Macro.bind macro code >>| fun bs -> macro,bs))

  let overload_defun arch args s3 =
    List.filter_map s3 ~f:(fun (def,cs) ->
        Option.(List.zip def.code.args args >>| fun bs -> def,bs))

  let overload_builtin s3 = s3

  let locs = List.map ~f:(fun (def,_) -> def.meta.loc)

  let run overload ctxts defs name =
    let s1 = stage1 defs name in
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


  let defun ctxts defs name arch args =
    run (overload_defun arch args) ctxts defs name

  let macro ctxts defs name code =
    run (overload_macro code) ctxts defs name

  let builtin ctxts defs name =
    run overload_builtin ctxts defs name
end

module Type_annot = struct
  let parse sz = Type (int_of_string (String.strip sz))
end

module Variable = struct
  type t = var

  let parse x = match String.split x ~on:':' with
    | [x;sz] -> {value=x; typ = Type_annot.parse sz}
    | _ -> {value=x;typ=Word}

  let to_string = function
    | {value;typ = Word} -> value
    | {value;typ = Type n} -> sprintf "%s:%d" value n

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
  open Sexp

  let atom = function
    | Atom x -> x
    | s -> expects "atom" s

  let typ = Type_annot.parse

  let char x = Int {
      value = Int64.of_int (Char.to_int (Char.of_string x));
      typ = Type 8
    }

  let word x =
    if Char.(x.[0] = '?') then char (String.subo ~pos:1 x)
    else match String.split x ~on:':' with
    | [x] ->  Int {value=Int64.of_string x; typ=Word}
    | [x;sz] -> Int {
        value = Int64.of_string (String.strip x);
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
    | _ -> bop_of_sexp op

  let is_bop x = try ignore (bop x); true with exn -> false

  let bop x e1 e2 = Bop (bop x, e1, e2)

  let handle_error _ = assert false

  let keywords = [
    "if"; "let"; "neg"; "not"; "coerce"]
  let is_keyword op = List.mem keywords op
  let nil = Int {value=0L; typ=Word}

  let macros : macro def list ref = ref []

  let is_macro op =
    List.exists macros.contents ~f:(fun m -> String.(m.meta.name = op))

  let subst ctxts name ops =
    match Resolve.macro ctxts !macros name ops with
    | _,None -> List [Atom "error"; Atom "unresolved macro"]
    | _,Some (macro,bs) -> Macro.apply macro bs

  let exp ctxts =
    let rec exp = function
      | Atom x when is_word x -> word x
      | Atom x -> Var (var x)
      | List [Atom "if"; c; e1; e2] ->
        Ite (exp c, exp e1, exp e2)
      | List (Atom "let" :: List bs :: e) -> let' bs e
      | List [Atom "coerce"; e1; e2; e3] ->
        Ext (exp e1, exp e2, exp e3)
      | List [Atom "neg"; e] -> Uop (Neg, exp e)
      | List [Atom "not"; e] -> Uop (Not, exp e)
      | List (Atom "prog" :: es) -> Seq (exps es,None)
      | List (Atom "while" :: c :: es) -> Rep (exp c, Seq (exps es,None))
      | List [Atom "set"; Atom x; e] -> Set (var x, exp e)
      | List (Atom op ::_) when is_keyword op -> handle_error exp
      | List (op :: arg :: args) when is_bop op ->
        List.fold ~f:(bop op) ~init:(exp arg) (exps args)
      | List (Atom op :: exps) when is_macro op ->
        exp (subst ctxts op exps)
      | List (Atom op :: args) -> App (op, exps args)
      | List [] -> nil
      | s ->  expects "(<ident> exps..)" s
    and exps = List.map ~f:exp
    and let' bs e =
      List.fold_right bs ~init:(Seq (exps e,None)) ~f:(fun b e ->
          match b with
          | List [Atom v; x] -> Let (var v,exp x,e)
          | s -> expects "(var exp)" s) in
    exp

  let params = function
    | List vars -> List.map ~f:(fun x -> var (atom x)) vars
    | s -> expects "(v1 v2 ..)" s

  let metaparams = function
    | List vars -> List.map ~f:atom vars
    | s -> expects "(s1 s2 ..)" s

  module State = struct
    type t = {
      global_attrs : attrs;
      parse_module : string -> t -> t;
      defs : func defs;
      current : filepos;
      features : String.Set.t;
      constraints : Contexts.t;
    }
  end
  open State

  let add_def defs def = def :: defs

  let parse_declarations attrs =
    List.fold ~init:attrs ~f:Attrs.parse

  let defun ?(docs="undocumented") ?(attrs=[]) name p body state = {
    state with
    defs = add_def state.defs {
        meta = {
          name;
          docs;
          attrs = parse_declarations state.global_attrs attrs;
          hooks = [];
          loc = Filepos state.current;
        };
        code = {
          args=params p;
          body = List.map ~f:(exp state.constraints) body
        }
      }
  }

  let defmacro ?(docs="undocumented") ?(attrs=[]) name ps body state =
    macros := {
      meta = {
        name;
        docs;
        attrs = parse_declarations state.global_attrs attrs;
        hooks = [];
        loc = Filepos state.current;
      };
      code = {
        param = metaparams ps;
        subst  = body;
      }
    } :: !macros;
    state

  let add_advice ~advisor ~advised where state = {
    state with
    defs =
      List.map state.defs ~f:(fun def ->
          if String.(def.meta.name = advised) then {
            def with meta = {
              def.meta with hooks = (where,advisor) :: def.meta.hooks
            }} else def)
  }

  let stmt state = function
    | List (Atom "defun" ::
            Atom name ::
            params ::
            List (Atom "declare" :: attrs) ::
            Atom docs ::
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
            Atom docs ::
            body) ->
      defun ~docs name params body state
    | List (Atom "defun" ::
            Atom name ::
            params ::
            body) ->
      defun name params body state
    | List [Atom "defmacro";
            Atom name;
            params;
            List (Atom "declare" :: attrs);
            Atom docs;
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
    | List [Atom "advice"; h; Atom f1; Atom ":with"; Atom f2] ->
      add_advice ~advised:f1 ~advisor:f2 (hook_of_sexp h) state
    | List (Atom "declare" :: attrs) -> {
        state with global_attrs =
                     parse_declarations state.global_attrs attrs
      }
    | List [Atom "require"; Atom name] ->
      state.parse_module name state
    | s -> expects "(defun ...) | (defmacro ..)" s


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
    eprintf "looking %s in %s\n" feature (String.concat paths ~sep:":");
    let name = feature ^ ".lisp" in
    List.find_map paths ~f:(fun path ->
        Sys.readdir path |> Array.find_map ~f:(fun file ->
            if String.(file = name)
            then Some (Filename.concat path file)
            else None))

  let feature
      ?(features=String.Set.empty)
      ?(paths=[Filename.current_dir_name])
      ?(defs=[]) cs feature =
    let open Parse.State in
    let rec load features defs feature =
      if Set.mem features feature then features,defs
      else match file_of_feature paths feature with
        | Some name ->
          let state = Parse.State.{
              current = make_loc dummy_range name;
              global_attrs = Univ_map.empty;
              defs;
              constraints=cs;
              features = Set.add features feature;
              parse_module = (fun feature s ->
                  let features,defs = load s.features s.defs feature in
                  {s with features; defs})
            } in
          let result = Parse.defs state (load_sexps name) in
          result.features, result.defs
        | None ->
          invalid_argf "Lisp loader: can't find module %s"
            feature () in
    load features defs feature

end


module State = Primus_state

module type Builtins = functor (Machine : Machine) ->  sig
  val defs : unit -> (Word.t list -> (Word.t,#Context.t) Machine.t) defs
end

type state = {
  builtins : (module Builtins);
  modules : String.Set.t;
  defs : func defs;
  width : int;
  env : (var * Word.t) list;
  contexts : Contexts.t;
  paths : string list;
}


let inspect_def {meta={name; docs}} = Sexp.List [
    Sexp.Atom name;
    Sexp.Atom docs;
  ]

let inspect {defs} =
  Sexp.List (List.map defs ~f:inspect_def)

let width_of_ctxt ctxt =
  Size.in_bits (Arch.addr_size (Project.arch ctxt#project))

let builtin
    ?(attrs=Univ_map.empty)
    ?(hooks=[])
    ?(docs="undocumented") name code =
  {meta = {name;docs; hooks; attrs; loc=Builtin}; code}

module Builtins(Machine : Machine) = struct
  open Machine.Syntax
  let all f args = Machine.return (Word.of_bool (List.exists args ~f))
  let is_zero args = all Word.is_zero args
  let is_positive args = all Word.is_positive args
  let is_negative args = all Word.is_negative args
  let word_width args =
    Machine.get () >>| width_of_ctxt >>| fun width ->
    match args with
    | [] -> Word.of_int ~width width
    | x :: xs -> Word.of_int ~width (Word.bitwidth x)


  let negone = Word.ones 8
  let zero = Word.zero 8

  let exit _ =
    Machine.update (fun ctxt -> ctxt#set_next None) >>= fun () ->
    Machine.return Word.b0

  let abort = exit


  let defs () = [
      builtin "is-zero" is_zero;
      builtin "is-positive" is_positive;
      builtin "is-negative" is_negative;
      builtin "word-width"  word_width;
      builtin "abort" abort;
      builtin "exit" exit;
  ]
end

let default_search_paths = [
  Filename.current_dir_name;
]

let state = Primus_state.declare ~inspect
    ~name:"lisp-library"
    ~uuid:"fc4b3719-f32c-4d0f-ad63-6167ab00b7f9"
    (fun ctxt -> {
         env = [];
         builtins = (module Builtins);
         modules = String.Set.empty;
         defs = [];
         paths = [Filename.current_dir_name];
         width = width_of_ctxt ctxt;
         contexts = Contexts.of_project ctxt#project;
       })

type error += Runtime_error of string
type error += Abort of int


let bil_of_lisp op =
  let open Bil in
  let binop op e1 e2 = BinOp (op,e1,e2) in
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
  | Cat  -> concat
  | Eq   -> binop eq
  | Le   -> binop le




module Machine(Machine : Machine) = struct
  open Machine.Syntax

  module Linker = Primus_linker.Make(Machine)

  let getenv () =
    Machine.Local.get state >>| fun s -> s.env

  let putenv env =
    Machine.Local.update state ~f:(fun s -> {s with env})

  let rec env_update xs x ~f = match xs with
    | [] -> []
    | (x',w) :: xs when x' = x -> (x,f w) :: xs
    | xw :: xs -> xw :: env_update xs x ~f

  let env_replace xs x w = env_update xs x ~f:(fun _ -> w)

  module Expi = Expi.Make(Machine)

  let word width value typ =
    let width = match typ with
      | Word -> width
      | Type n -> n in
    Word.of_int64 ~width value

  let var width {value;typ} =
    let typ = match typ with
      | Word -> Type.Imm width
      | Type n -> Type.Imm n in
    Var.create value typ

  let width () = Machine.Local.get state >>| fun {width} -> width

  let eval_builtin name args =
    Machine.Local.get state >>= fun {contexts; builtins=(module Make)} ->
    let module Builtins = Make(Machine) in
    match Resolve.builtin contexts (Builtins.defs ()) name with
    | _,None -> Linker.exec (`symbol name) >>= fun () ->
      Machine.return Word.b0
    | _,Some (def,_) -> def.code args

  let rec eval_lisp biri name args =
    Machine.get () >>= fun ctxt ->
    let arch = Project.arch ctxt#project in
    Machine.Local.get state >>= fun s ->
    match Resolve.defun s.contexts s.defs name arch args with
    | {Resolve.stage1=[]},None -> eval_builtin name args
    | resolution,None -> Machine.fail (Resolve.Failed resolution)
    | _,Some (fn,bs) ->
      Machine.Local.put state {
        s with env = bs @ s.env
      } >>= fun () ->
      eval_body biri fn.code.body

  and eval_body biri body = eval_exp biri (Seq (body,None))

  and eval_exp biri  exp : (Word.t,#Context.t) Machine.t =
    let int v t = width () >>| fun width -> word width v t in
    let rec eval = function
      | Int {value;typ} -> int value typ
      | Var v -> lookup v
      | Ite (c,e1,e2) -> ite c e1 e2
      | Let (v,e1,e2) -> let_ v e1 e2
      | Ext (lo,hi,e) -> ext lo hi e
      | App (n,args) -> app n args
      | Rep (c,e) -> rep c e
      | Bop (op,e1,e2) -> bop op e1 e2
      | Uop (op,e) -> uop op e
      | Seq (es,short) -> seq es short
      | Set (v,e) -> eval e >>= set v
      | Err msg -> Machine.fail (Runtime_error msg)
    and rep c e =
      eval c >>= fun r -> if Word.is_zero r then Machine.return r
      else eval e >>= fun _ -> rep c e
    and ite c e1 e2 =
      eval c >>= fun w -> if Word.is_zero w then eval e2 else eval e1
    and let_ v e1 e2 =
      eval e1 >>= fun w ->
      Machine.Local.update state (fun s -> {s with env = (v,w)::s.env})
      >>=  fun () -> eval e2
    and ext lo hi w =
      let eval_to_int e =
        eval e >>| Word.to_int >>= function
        | Ok n -> Machine.return n
        | Error _ -> Machine.fail (Runtime_error "expected smallint") in
      eval_to_int lo >>= fun lo ->
      eval_to_int hi >>= fun hi ->
      eval w >>= fun w ->
      cast (biri#eval_extract lo hi (Bil.Int w))
    and cast r = r >>= fun r -> match Bil.Result.value r with
      | Bil.Bot -> Machine.fail (Runtime_error "got bot")
      | Bil.Mem _ -> Machine.fail (Runtime_error "got mem")
      | Bil.Imm w -> Machine.return w
    and lookup v =
      Machine.Local.get state >>= fun {env; width} ->
      match List.Assoc.find env v with
      | Some w -> Machine.return w
      | None -> cast (biri#lookup (var width v))
    and app n args =
      Machine.List.map args ~f:eval >>= eval_lisp biri n
    and seq es short =
     let rec loop f = function
      | [] -> Machine.return Word.b0
      | e :: [] -> eval e
      | e :: es -> eval e >>= fun r ->
        if f (Word.is_zero r)
        then Machine.return Word.b0
        else loop f es in
     loop (fun r -> Some r = short) es
    and set v w =
      Machine.Local.get state >>= fun s ->
      if List.Assoc.mem s.env v
      then
        Machine.Local.put state {s with env = env_replace s.env v w}
        >>= fun () -> Machine.return w
      else
        biri#eval_int w >>= fun r ->
        biri#update (var s.width v) r >>= fun () ->
        Machine.return w
    and bop op e1 e2 =
      eval e1 >>= fun e1 ->
      eval e2 >>= fun e2 ->
      cast @@ biri#eval_exp (bil_of_lisp op (Bil.int e1) (Bil.int e2))
    and uop op e =
      eval e >>= fun e ->
      let op = match op with
        | Neg -> Bil.NEG
        | Not -> Bil.NOT in
      cast @@ biri#eval_exp Bil.(UnOp (op,Int e)) in
    eval exp

  let run (biri : #Context.t #Expi.t) name args =
    eval_lisp biri name args

  let is_bound name =
    Machine.Local.get state >>| fun {defs} ->
    List.exists defs ~f:(fun def ->
        match Univ_map.find def.meta.attrs External.t with
        | Some names -> List.mem names name
        | None -> false)

  let load_feature name =
    Machine.Local.update state ~f:(fun s ->
        let modules,defs = Load.feature
            ~paths:s.paths
            ~features:s.modules
            ~defs:s.defs s.contexts name in
        {s with modules; defs})


  let add_directory path =
    Machine.Local.update state ~f:(fun s -> {
          s with paths = path :: s.paths
        })

end
