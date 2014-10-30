open Core_kernel.Std
open Bap_common
open Format

let rec pp fmt s =
  let open Bap_bil.Stmt in match s with
  | Move (var, exp) -> fprintf fmt "%a = %a" Bap_var.pp var Bap_exp.pp exp
  | Jmp exp -> fprintf fmt "jmp %a" Bap_exp.pp exp
  | Special s -> fprintf fmt "special (%s)" s
  | While (cond, body) ->
    fprintf fmt "@[<v0>@[<v2>while (%a) {@;%a@]@\n}@]"
      Bap_exp.pp cond pp_list body
  | If (cond, ts, fs) ->
    fprintf fmt "@[<v0>@[<v2>if (%a) {@;%a@]@,}@\n%a@]"
      Bap_exp.pp cond pp_list ts pp_else fs
  | CpuExn  n -> fprintf fmt "cpuexn (%d)" n
and pp_list fmt = function
  | [] -> ()
  | x :: [] -> fprintf fmt "%a" pp x
  | x :: xs -> fprintf fmt "%a@;%a" pp x pp_list xs
and pp_else fmt = function
  | [] -> ()
  | fs -> fprintf fmt "@[<v0>@[<v2>else {@;%a@]@\n}@]" pp_list fs

let pp_stmts fmt ss =
  fprintf fmt "@[<v0>@[<v2>{@\n%a@]@\n}@]" pp_list ss

include Regular.Make(struct
    include Bap_bil.Stmt
    let hash = Hashtbl.hash
    let module_name = "Bap_stmt"
    let pp = pp
  end)
