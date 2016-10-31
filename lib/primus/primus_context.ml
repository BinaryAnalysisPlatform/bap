open Core_kernel.Std
open Bap.Std

open Primus_types

module Machine = Primus_machine
module Error = Primus_error

module Level = struct
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

  type name =
    [`top | `sub | `arg | `blk | `phi | `def | `jmp]
  [@@deriving sexp]

  type invariant = {
    level : t;
    dst : name
  }

  type Error.t += Broken_invariant of invariant

  let string_of_level = function
    | Top _ -> "top"
    | Sub _ -> "sub"
    | Arg _ -> "arg"
    | Blk _ -> "blk"
    | Phi _ -> "phi"
    | Def _ -> "def"
    | Jmp _ -> "jmp"

  let () = Error.add_printer (function
      | Broken_invariant {level; dst} -> Option.some @@ sprintf
          "Level transition - broken invariant: \
           No transition is defined from the %s level to the %s level"
          (string_of_level level)
          (string_of_sexp (sexp_of_name dst))
      | exn -> None)


  (* let next level cls t  = *)
  (*   Term.switch cls t *)
  (*     ~program:(fun p -> match level with *)
  (*         | Top _ -> Ok (Top {me=p; up=Nil}) *)
  (*         | _ -> None) *)
  (*     ~sub:(fun sub -> match level with *)
  (*         | Top top | Sub {up=top} -> *)
  (*           Some {< level = Sub {me=sub; up=top} >} *)
  (*         | _ -> None) *)
  (*     ~arg:(fun arg -> match level with *)
  (*         | Sub sub | Blk {up=sub} -> *)
  (*           Some {< level = Arg {me=arg; up=sub} >} *)
  (*         | _ -> None) *)
  (*     ~blk:(fun blk -> match level with *)
  (*         | Blk {up=sub} | Sub sub | Arg {up=sub} -> *)
  (*           Some {< level = Blk {me=blk; up=sub} >} *)
  (*         | _ -> None) *)
  (*     ~phi:(fun phi -> match level with *)
  (*         | Blk blk | Phi {up=blk} -> *)
  (*           Some {< level = Phi {me=phi; up=blk} >} *)
  (*         | _ -> None) *)
  (*     ~def:(fun def -> match level with *)
  (*         | Blk blk | Phi {up=blk} | Def {up=blk}-> *)
  (*           Some {< level = Def {me=def; up=blk} >} *)
  (*         | _ -> None) *)
  (*     ~jmp:(fun jmp -> match level with *)
  (*         | Blk blk | Phi {up=blk} | Def {up=blk} | Jmp {up=blk} -> *)
  (*           Some {< level = Jmp {me=jmp; up=blk} >} *)
  (*         | _ -> None) *)

end

open Level

class t ?main proj =
  let prog = Project.program proj in
  object(self : 's)
    inherit Biri.context ?main prog
    val level = Top {me=prog; up=Nil}
    method project = proj
    method curr =
      let (!) {me} = Term.tid me in
      match level with
      | Top t -> !t | Sub t -> !t | Arg t -> !t | Blk t -> !t
      | Phi t -> !t | Def t -> !t | Jmp t -> !t
  end
