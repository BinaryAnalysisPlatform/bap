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
type var = string scalar[@@derving sexp]

type exp =
  | Int of word
  | Var of var
  | Ite of exp * exp * exp
  | Let of var * exp * exp
  | Ext of int * int * exp
  | Bop of bop * exp * exp
  | Uop of uop * exp
  | App of string * exp list
  | Seq of exp list * bool option
  | Set of var * exp
  | Rep of exp * exp


type hook = [`enter | `leave] [@@deriving sexp]
type advice = {
  advised : string;
  advisor : string;
  where : hook;
}

type attr = {
  attr : string;
  vals : string list
}

type meta = {
  name : string;
  docs : string;
  attrs : attr list;
  hooks : (hook * string) list;
}

type func = {
  args : var list;
  body : exp list;
}

type 'a def = {meta : meta; code : 'a}
type 'a defs = Defs of 'a def list

type stmt =
  | Advice of advice
  | Defun  of func def
  | Attrs  of attr list
  | Macro

module Parse = struct
  open Sexp

  let expect what got =
    invalid_argf "Parser error: expected %s, got %s"
      what got ()

  let expects what got = expect what (Sexp.to_string got)

  let atom = function
    | Atom x -> x
    | s -> expects "int" s

  let typ sz = Type (int_of_string (String.strip sz))

  let word x = match String.split x ~on:':' with
    | [x] ->  Int {value=Int64.of_string x; typ=Word}
    | [x;sz] -> Int {
        value = Int64.of_string (String.strip x);
        typ   = typ sz
      }
    | _ -> expect "int ::= <lit> | <lit>:<typ>" x


  let is_word x = try ignore (word x);true with _ -> false
  let var x = match String.split x ~on:':' with
    | [x;sz] -> {value=x; typ = typ sz}
    | _ -> {value=x;typ=Word}

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
  let is_keyword op = List.mem ["if"; "let"; "neg"; "not"; "bits"] op
  let nil = Int {value=0L; typ=Word}

  let negate = function
    | [] -> expect "(/= e1 e2 ..)" "(/=)"
    | exps -> List [Atom "not"; List (Atom "=" :: exps)]

  let macro = ref [
    "/=", negate;
  ]

  let is_macro op = List.Assoc.mem !macro op

  let add_macro (name : string) (ps : string list) (body) : unit =
    let rec bind ps cs = match ps,cs with
      | [p],cs -> [p,cs]
      | (p::ps),(c::cs) -> (p,[c]) :: bind ps cs
      | _ -> invalid_arg "invalid macro arity" in
    let rec subst bs : Sexp.t -> Sexp.t = function
      | List xs -> List (List.concat_map xs ~f:(function
          | List xs -> [list bs xs]
          | Atom x -> atom bs x))
      | Atom x -> Atom x
    and list bs xs = List (List.map ~f:(subst bs) xs)
    and atom bs x = match List.Assoc.find bs x with
        | None -> [Atom x]
        | Some cs -> cs in
    let apply code = subst (bind ps code) body in
    macro := List.Assoc.add !macro name apply

  let subst name =
    (List.Assoc.find_exn !macro name)

  let rec exp = function
    | Atom x when is_word x -> word x
    | Atom x -> Var (var x)
    | List [Atom "if"; c; e1; e2] ->
      Ite (exp c, exp e1, exp e2)
    | List (Atom "let" :: List bs :: e) -> let' bs e
    | List [Atom "coerce"; e1; e2; e3] ->
      Ext (int e1, int e2, exp e3)
    | List [Atom "neg"; e] -> Uop (Neg, exp e)
    | List [Atom "not"; e] -> Uop (Not, exp e)
    | List (Atom "prog" :: es) -> Seq (exps es,None)
    | List (Atom "while" :: c :: es) -> Rep (exp c, Seq (exps es,None))
    | List [Atom "set"; Atom x; e] -> Set (var x, exp e)
    | List (Atom op ::_) when is_keyword op -> handle_error exp
    | List (op :: arg :: args) when is_bop op ->
      List.fold ~f:(bop op) ~init:(exp arg) (exps args)
    | List (Atom op :: exps) when is_macro op ->
      exp (subst op exps)
    | List (Atom op :: args) -> App (op, exps args)
    | List [] -> nil
    | s ->  expects "(<ident> exps..)" s
  and exps = List.map ~f:exp
  and let' bs e =
    List.fold_right bs ~init:(Seq (exps e,None)) ~f:(fun b e ->
        match b with
        | List [Atom v; x] -> Let (var v,exp x,e)
        | s -> expects "(var exp)" s)

  let params = function
    | List vars -> List.map ~f:(fun x -> var (atom x)) vars
    | s -> expects "(v1 v2 ..)" s

  let metaparams = function
    | List vars -> List.map ~f:atom vars
    | s -> expects "(s1 s2 ..)" s

  let parse_attrs = List.map ~f:(function
      | Atom x -> {attr=x; vals=[]}
      | List [Atom x; Atom v] -> {attr=x; vals=[v]}
      | List [Atom x; List vs] -> {attr=x; vals=List.map ~f:atom vs}
      | s -> expects "v | (v x) | (v (x1 ... xm)" s)


  let defun ?(docs="undocumented") ?(attrs=[]) name p body = Defun {
    meta = {name; docs; attrs = parse_attrs attrs; hooks = []};
    code = {args=params p; body = List.map ~f:exp body}
  }



  let stmt = function
    | List (Atom "defun" :: Atom n :: p ::
            List (Atom "declare" :: attrs) :: Atom docs :: b) ->
      defun ~docs ~attrs n p b
    | List (Atom "defun" :: Atom n :: p ::
            List (Atom "declare" :: attrs) :: b) ->
      defun ~attrs n p b
    | List (Atom "defun" :: Atom n :: p ::
            Atom docs :: b) ->
      defun ~docs n p b
    | List (Atom "defun" :: Atom n :: p :: b) -> defun n p b
    | List [Atom "defmacro"; Atom name; p; Atom _; body]
    | List [Atom "defmacro"; Atom name; p; body] ->
      add_macro name (metaparams p) body; Macro
    | List [Atom "advice"; h; Atom f1; Atom "with"; Atom f2] ->
      Advice {advised=f1; advisor=f2; where = hook_of_sexp h}
    | List (Atom "declare" :: attrs) -> Attrs (parse_attrs attrs)
    | s -> expects "(defun ...) | (defmacro ..)" s


  let advice ads =
    List.map ~f:(fun def ->
        List.fold ads ~init:def ~f:(fun def {where; advised; advisor} ->
            if String.(def.meta.name = advised) then {
              def with meta = {
                def.meta with hooks = (where,advisor) :: def.meta.hooks
              }} else def))

    let attribute attrs =
      List.map ~f:(fun def ->
          {def with meta = {def.meta with attrs = def.meta.attrs @ attrs }})


  let defs sexps =
    let stmts = List.map ~f:stmt sexps in
    let ads,defs,attrs =
      List.fold stmts ~init:([],[],[]) ~f:(fun (ads,defs,attrs) -> function
          | Advice a -> a::ads,defs,attrs
          | Defun d  -> ads,d::defs,attrs
          | Attrs a  -> ads,defs,a@attrs
          | Macro    -> ads,defs,attrs) in
    defs |> attribute attrs |> advice ads


  let string data = defs (scan_sexps (Lexing.from_string data))
  let file name = defs (Sexp.load_sexps name)
end


module State = Primus_state


module type Builtins = functor (Machine : Machine) ->  sig
  val defs : unit -> (Word.t list -> (Word.t,#Context.t) Machine.t) defs
end


type state = {
  builtins : (module Builtins);
  defs : func defs;
  width : int;
  env : (var * Word.t) list;
}


let inspect_def {meta={name; docs}} = Sexp.List [
    Sexp.Atom name;
    Sexp.Atom docs;
  ]

let inspect {defs = Defs ds} =
  Sexp.List (List.map ds ~f:inspect_def)

let width_of_ctxt ctxt =
  Size.in_bits (Arch.addr_size (Project.arch ctxt#project))

let builtin ?(attrs=[]) ?(hooks=[]) ?(docs="undocumented") name code =
  {meta = {name;docs; hooks; attrs}; code}

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

  let defs () = Defs [
      builtin "is-zero" is_zero;
      builtin "is-positive" is_positive;
      builtin "is-negative" is_negative;
      builtin "word-width"  word_width;
  ]
end

let state = Primus_state.declare ~inspect
    ~name:"lisp-library"
    ~uuid:"fc4b3719-f32c-4d0f-ad63-6167ab00b7f9"
    (fun ctxt -> {
         env = [];
         builtins = (module Builtins);
         defs = Defs [];
         width = width_of_ctxt ctxt;
       })

type error += Runtime_error


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

  let getenv () =
    Machine.Local.get state >>| fun s -> s.env

  let putenv env =
    Machine.Local.update state ~f:(fun s -> {s with env})

  let rec env_update xs x ~f = match xs with
    | [] -> []
    | (x',w) :: xs when x' = x -> (x,f w) :: xs
    | xw :: xs -> xw :: env_update xs x ~f

  let env_replace xs x w = env_update xs x ~f:(fun _ -> w)

  module Biri = Primus_interpreter.Make(Machine)

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

  let resolve_name (Defs defs) name args =
    match List.find defs ~f:(fun d -> d.meta.name = name) with
    | None -> None
    | Some {code} -> Some code

  let eval_builtin name args =
    Machine.Local.get state >>= fun {builtins=(module Builtins)} ->
    let module Builtins = Builtins(Machine) in
    match resolve_name (Builtins.defs ()) name args with
    | None -> Machine.fail Runtime_error
    | Some code -> code args

  let rec eval_lisp biri name args =
    Machine.Local.get state >>= fun state ->
    match resolve_name state.defs name args with
    | None -> eval_builtin name args
    | Some fn -> match List.zip fn.args args with
      | None -> assert false
      | Some bs -> eval_body biri fn.body

  and eval_body biri body = eval_exp biri (Seq (body,None))

  and eval_exp (biri : 'a #Biri.t) exp : (Word.t,#Context.t) Machine.t =
    let int v t = width () >>| fun width -> word width v t in
    let rec eval = function
      | Int {value;typ} -> int value typ
      | Var v -> lookup v
      | Ite (c,e1,e2) -> ite c e1 e2
      | Let (v,e1,e2) -> let_ v e1 e2
      | Ext (lo,hi,e) -> eval e >>= ext lo hi
      | App (n,args) -> app n args
      | Rep (c,e) -> rep c e
      | Bop (op,e1,e2) -> bop op e1 e2
      | Uop (op,e) -> uop op e
      | Seq (es,short) -> seq es short
      | Set (v,e) -> eval e >>= set v
    and rep c e =
      eval c >>= fun r -> if Word.is_zero r then Machine.return r
      else eval e >>= fun _ -> rep c e
    and ite c e1 e2 =
      eval c >>= fun w -> if Word.is_zero w then eval e2 else eval e1
    and let_ v e1 e2 =
      eval e1 >>= fun w ->
      Machine.Local.update state (fun s -> {s with env = (v,w)::s.env})
      >>=  fun () -> eval e2
    and ext lo hi w = cast (biri#eval_extract lo hi (Bil.Int w))
    and cast r = r >>= fun r -> match Bil.Result.value r with
      | Bil.Bot -> Machine.fail Runtime_error
      | Bil.Mem _ -> Machine.fail Runtime_error
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



end
