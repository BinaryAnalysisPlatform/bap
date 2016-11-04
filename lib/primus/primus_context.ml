open Core_kernel.Std
open Bap.Std

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
  [@@deriving variants]

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

  let accept level args = Ok (level args)
  let reject level dst = Error (Broken_invariant {level; dst})

  let next level cls t  =
    let reject = reject level in
    Term.switch cls t
      ~program:(fun p -> match level with
          | Top _ -> accept top {me=p; up=Nil}
          | _ -> reject `top)
      ~sub:(fun me -> match level with
          | Top up | Sub {up} -> accept sub {me; up}
          | _ -> reject `sub)
      ~arg:(fun me -> match level with
          | Sub up | Blk {up} -> accept arg {me;up}
          | _ -> reject `arg)
      ~blk:(fun me -> match level with
          | Blk {up} | Sub up | Arg {up} -> accept blk {me;up}
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

end

open Level

class t ?main proj =
  let prog = Project.program proj in
  object(self : 's)
    inherit Biri.context ?main prog
    val level = Top {me=prog; up=Nil}
    method project = proj
    method with_level level = {< level = level >}
    method curr =
      let (!) {me} = Term.tid me in
      match level with
      | Top t -> !t | Sub t -> !t | Arg t -> !t | Blk t -> !t
      | Phi t -> !t | Def t -> !t | Jmp t -> !t
  end
