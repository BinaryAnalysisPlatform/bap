open Core_kernel.Std
open Bap_common
open Format
open Bap_bil

let rec pp fmt s =
  let open Stmt in match s with
  | Move (var, exp) ->
    fprintf fmt "@[<v2>%a := %a@]" Bap_var.pp var Bap_exp.pp exp
  | Jmp (Exp.Var _ | Exp.Int _ as exp) ->
    fprintf fmt "jmp %a" Bap_exp.pp exp
  | Jmp exp -> fprintf fmt "jmp (%a)" Bap_exp.pp exp
  | Special s -> fprintf fmt "special (%s)" s
  | While (cond, body) ->
    fprintf fmt "@[<v0>@[<v2>while (%a) {@;%a@]@;}@]"
      Bap_exp.pp cond pp_list body
  | If (cond, ts, []) ->
    fprintf fmt "@[<v0>@[<v2>if (%a) {@;%a@]@,}@]"
      Bap_exp.pp cond pp_list ts
  | If (cond, ts, fs) ->
    fprintf fmt "@[<v0>@[<v2>if (%a) {@;%a@]@,}@;%a@]"
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

module Stmt = struct
  open Bap_bil.Stmt
  let move v x = Move (v,x)
  let jmp x = Jmp x
  let special s = Special s
  let while_ x s1  = While (x,s1)
  let if_ x s1 s2 = If (x,s1,s2)
  let cpuexn n = CpuExn n
end


module Infix = struct
  let (:=) v x = Bap_bil.Stmt.Move (v,x)
end

include Regular.Make(struct
    type t = Bap_bil.stmt with bin_io, compare, sexp
    let hash = Hashtbl.hash
    let module_name = "Bap_stmt"
    let pp = pp
  end)

module Stmts_pp = struct
  type t = stmt list
  include Printable(struct
      type nonrec t = t
      let pp = pp_stmts
      let module_name = "Bil"
    end)
end
