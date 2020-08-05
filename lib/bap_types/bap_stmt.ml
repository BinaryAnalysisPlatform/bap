open Core_kernel
open Bap_core_theory
open Bap_knowledge
open Regular.Std
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
    type t = Bap_bil.stmt [@@deriving bin_io, compare, sexp]
    let hash = Hashtbl.hash
    let module_name = Some "Bap.Std.Stmt"
    let version = "1.0.0"

    let pp = pp
  end)

module Stmts_pp = struct
  type t = stmt list
  include Printable.Make(struct
      type nonrec t = t
      let pp = pp_stmts
      let module_name = Some "Bap.Std.Bil"
    end)
end

module Stmts_data = struct
  module T = struct
    type t = stmt list [@@deriving bin_io, sexp]
    let version = "1.0.0"
  end
  include T
  include Data.Make(T)
  open Data
  let bin_reader = bin_reader (module T)
  let bin_writer = bin_writer (module T)
  let sexp_reader = sexp_reader (module T)
  let sexp_writer = sexp_writer (module T)
  let printer = (Data.pretty_writer (module Stmts_pp))

  let () =
    let ver = version in
    Data.set_module_name instance "Bap.Std.Bil";
    add_writer ~desc:"Janestreet Binary Protocol" ~ver "bin" bin_writer;
    add_reader ~desc:"Janestreet Binary Protocol" ~ver "bin" bin_reader;
    add_writer ~desc:"Janestreet Sexp Protocol" ~ver "sexp" sexp_writer;
    add_reader ~desc:"Janestreet Sexp Protocol" ~ver "sexp" sexp_reader;
    add_writer ~desc:"Pretty printer" ~ver:T.version "pretty" printer;
    set_default_printer "pretty";
    set_default_writer "bin";
    set_default_reader "bin"
end


let domain = Knowledge.Domain.flat "bil"
    ~empty:[]
    ~inspect:(function
        | [] -> Sexp.List []
        | bil -> Sexp.Atom (Stmts_pp.to_string bil))
    ~equal:(fun x y ->
        phys_equal x y ||
        Int.(compare_bil x y = 0))


let persistent = Knowledge.Persistent.of_binable (module struct
    type t = stmt list [@@deriving bin_io]
  end)

let slot = Knowledge.Class.property ~package:"bap"
    ~persistent Theory.Program.Semantics.cls "bil" domain
    ~public:true
    ~desc:"semantics of statements in BIL"
