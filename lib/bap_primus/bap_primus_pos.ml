open Core_kernel
open Bap.Std

module Exn = Bap_primus_exn

type nil = Nil
type top = program

type ('a,'b) level = {
  me : 'a term;
  up : 'b;
}

type level3 = (top,nil) level
type level2 = (sub,level3) level
type 'a level1 = ('a,level2) level
type 'a level0 = ('a,blk level1) level

type t =
  | Top of level3
  | Sub of level2
  | Arg of arg level1
  | Blk of blk level1
  | Phi of phi level0
  | Def of def level0
  | Jmp of jmp level0
[@@deriving variants]

type name =
  [`top | `sub | `arg | `blk | `phi | `def | `jmp]
[@@deriving sexp]

type invariant = {
  level : t;
  dst : name
}

type Exn.t += Broken_invariant of invariant

let (^^) name {me} = sprintf "%s(%s)" name (Term.name me)

let to_string = function
  | Top t -> "top" ^^ t
  | Sub t -> "sub" ^^ t
  | Arg t -> "arg" ^^ t
  | Blk t -> "blk" ^^ t
  | Phi t -> "phi" ^^ t
  | Def t -> "def" ^^ t
  | Jmp t -> "jmp" ^^ t

let () = Exn.add_printer (function
    | Broken_invariant {level; dst} -> Option.some @@ sprintf
        "Level transition - broken invariant: \
         No transition is defined from the %s level to the %s level"
        (to_string level)
        (string_of_sexp (sexp_of_name dst))
    | exn -> None)

let accept level args = Ok (level args)
let reject level dst = Error (Broken_invariant {level; dst})


let level name f x = Sexp.List [
    Sexp.Atom name;
    Sexp.Atom (f x)
]
let (>>) = Fn.compose
let leaf name str t = Sexp.List [
    Sexp.Atom name;
    Sexp.Atom (String.strip (str t));
  ]

let sexp_of_t = function
  | Top _   -> Sexp.Atom "top"
  | Sub {me} -> level "sub" Sub.name me
  | Arg {me} -> level "arg" (Var.name >> Arg.lhs) me
  | Blk {me} -> level "blk" (Tid.name >> Term.tid ) me
  | Phi {me} -> leaf "phi" Phi.to_string me
  | Def {me} -> leaf "def" Def.to_string me
  | Jmp {me} -> leaf "jmp" Jmp.to_string me

let next level cls t  =
  let reject = reject level in
  Term.switch cls t
    ~program:(fun p -> match level with
        | Top _ -> accept top {me=p; up=Nil}
        | _ -> reject `top)
    ~sub:(fun me -> match level with
        | Top up | Sub {up} | Jmp {up={up={up}}} ->
          accept sub {me; up}
        | _ -> reject `sub)
    ~arg:(fun me -> match level with
        | Sub up | Arg {up} | Jmp {up={up}}  -> accept arg {me;up}
        | _ -> reject `arg)
    ~blk:(fun me -> match level with
        | Jmp {up={up}} | Sub up | Arg {up} -> accept blk {me;up}
        | _ -> reject `blk)
    ~phi:(fun me -> match level with
        | Blk up | Phi {up} -> accept phi {me;up}
        | _ -> reject `phi)
    ~def:(fun me -> match level with
        | Blk up | Phi {up} | Def {up} -> accept def {me;up}
        | _ -> reject `def)
    ~jmp:(fun me -> match level with
        | Blk up | Phi {up} | Def {up} | Jmp {up} ->
          accept jmp {me;up}
        | _ -> reject `jmp)


let tid level =
  let (!) {me} = Term.tid me in
  match level with
  | Top t -> !t | Sub t -> !t | Arg t -> !t | Blk t -> !t
  | Phi t -> !t | Def t -> !t | Jmp t -> !t

let get tag level =
  let (!) {me} = Term.get_attr me tag in
  match level with
  | Top t -> !t | Sub t -> !t | Arg t -> !t | Blk t -> !t
  | Phi t -> !t | Def t -> !t | Jmp t -> !t
