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
  | Seq of exp list
  | Set of var * exp

type func = {
  docs : string;
  args : var list;
  body : exp list;
}

type def = Def of string * func
type defs = func String.Map.t

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
    | List [Atom "let"; Atom x; e1; e2] ->
      Let (var x, exp e1, exp e2)
    | List [Atom "bits"; e1; e2; e3] ->
      Ext (int e1, int e2, exp e3)
    | List [Atom "neg"; e] -> Uop (Neg, exp e)
    | List [Atom "not"; e] -> Uop (Not, exp e)
    | List (Atom "prog" :: es) -> Seq (exps es)
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


  let params = function
    | List vars -> List.map ~f:(fun x -> var (atom x)) vars
    | s -> expects "(v1 v2 ..)" s

  let metaparams = function
    | List vars -> List.map ~f:atom vars
    | s -> expects "(s1 s2 ..)" s


  let defun ?(docs="undocumented") name p body =
    Def (name, {docs; args=params p; body=List.map ~f:exp body})

  let def = function
    | List (Atom "defun" :: Atom n :: p :: Atom docs :: b) ->
      Some (defun ~docs n p b)
    | List (Atom "defun" :: Atom n :: p :: b) -> Some (defun n p b)
    | List [Atom "defmacro"; Atom name; p; Atom _; body]
    | List [Atom "defmacro"; Atom name; p; body] ->
      add_macro name (metaparams p) body;
      None
    | s -> expects "(defun ...) | (defmacro ..)" s

  let defs = List.filter_map ~f:def
  let string data = defs (scan_sexps (Lexing.from_string data))
  let file name = defs (Sexp.load_sexps name)
end


module State = Primus_state


module type Syscall = functor (Machine : Machine) ->  sig
  val exec : Word.t list -> (Word.t,#Context.t) Machine.t
end


type syscall = {
  desc : string;
  code : (module Syscall);
}


type state = {
  kernel : syscall String.Map.t;
  defs : defs;
  width : int;
}


let inspect_def (name,{docs}) = Sexp.List [
    Sexp.Atom name;
    Sexp.Atom docs;
  ]

let inspect {defs} =
  Sexp.List Seq.(Map.to_sequence defs >>| inspect_def |> to_list)



let state = Primus_state.declare ~inspect
    ~name:"lisp-library"
    ~uuid:"fc4b3719-f32c-4d0f-ad63-6167ab00b7f9"
    (fun ctxt -> {
         kernel = String.Map.empty;
         defs = String.Map.empty;
         width = Size.in_bits (Arch.addr_size
                                 (Project.arch ctxt#project))
       })

type error += Runtime_error




module Machine(Machine : Machine) = struct
  open Machine.Syntax

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



  let eval_syscall name args =
    Machine.Local.get state >>= fun {kernel} ->
    match Map.find kernel name with
    | None -> Machine.fail Runtime_error
    | Some {code=(module Syscall)} ->
      let module Syscall = Syscall(Machine) in
      Syscall.exec args

  let rec eval_lisp biri name args =
    Machine.Local.get state >>= fun state ->
    match Map.find state.defs name with
    | None -> eval_syscall name args
    | Some func -> match List.zip func.args args with
      | None -> assert false
      | Some bs -> eval_body state biri bs func.body

  and eval_body state biri bs body =
    eval_exp state biri bs (Seq body)

  and eval_exp {width} (biri : 'a #Biri.t) bs exp : (Word.t,#Context.t) Machine.t =
    let rec eval bs = function
      | Int {value;typ} -> Machine.return (word width value typ)
      | Var v -> lookup width biri bs v
      | Ite (c,e1,e2) -> ite bs c e1 e2
      | Let (v,e1,e2) -> let_ bs v e1 e2
      | Ext (lo,hi,e) -> eval bs e >>= ext lo hi
      | App (n,args) -> app bs n args
      | Bop (_,_,_)
      | Uop (_,_) -> assert false
      | Seq es -> seq bs es
      | Set (v,e) -> eval bs e >>= set v
    and ite bs c e1 e2 =
      eval bs c >>= fun w ->
      if Word.is_zero w then eval bs e2 else eval bs e1
    and let_ bs v e1 e2 =
      eval bs e1 >>= fun w -> eval ((v,w)::bs) e2
    and ext lo hi w = cast (biri#eval_extract lo hi (Bil.Int w))
    and cast r = r >>= fun r -> match Bil.Result.value r with
      | Bil.Bot -> Machine.fail Runtime_error
      | Bil.Mem _ -> Machine.fail Runtime_error
      | Bil.Imm w -> Machine.return w
    and lookup width biri bs v =
      match List.Assoc.find bs v with
      | Some w -> Machine.return w
      | None -> cast (biri#lookup (var width v))
    and app bs n args =
      Machine.List.map args ~f:(eval bs) >>= eval_lisp biri n
    and seq bs es =
      let null = word width 0L Word in
      Machine.List.fold es ~init:null ~f:(fun _ exp -> eval bs exp)
    and set v w =
      let var = var width v in
      biri#eval_int w >>= fun r ->
      biri#update var r >>= fun () ->
      Machine.return w
    in
    eval bs exp




end
