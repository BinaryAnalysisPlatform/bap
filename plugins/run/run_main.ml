open Core_kernel.Std
open Bap.Std
open Primus.Std
open Monads.Std
include Self()
open Format

type value =
  | Data of string
  | Word of Generator.t


type parameters = {
 argv  : value list;
 envp  : (string * value) list;
 entry : string option;
 memory : (addr * value) list;
 stack  : value list;
 variables : (string * value) list;
}


module Parameters = struct
  open Sexp
  type t = parameters

  let int = int_of_string

  let parse_generator = function
    | List [Atom x; Atom y; Atom z] ->
      Generator.Random.lcg ~min:(int x) ~max:(int y) (int z)
    | List [Atom x; Atom y] ->
      Generator.Random.Seeded.lcg ~min:(int x) ~max:(int y) ()
    | Atom x -> Generator.static (int x)
    | _ -> invalid_arg "Parse error:
     expected 'value := const | (min max) | (min max seed)'"

  let int_literal x = try ignore (int x); true with exn -> false

  let parse_value = function
    | Atom x when not (int_literal x) -> Data x
    | gen -> Word (parse_generator gen)
end

module Mach = Machine.Make(Monad.Ident)
module Main = Machine.Main(Mach)
module Interpreter = Interpreter.Make(Mach)
module Linker = Linker.Make(Mach)
module Environment(Machine : Machine.S) = struct
end

let pp_backtrace ppf ctxt =
  let prog = ctxt#program in
  List.iter ctxt#trace ~f:(fun tid ->
      let do_if cls = Option.iter (Program.lookup cls prog tid) in
      let pp_if cls pp = do_if cls ~f:(fun t ->
          fprintf ppf "    %a" pp t) in
      pp_if arg_t Arg.pp;
      pp_if phi_t Phi.pp;
      pp_if def_t Def.pp;
      pp_if jmp_t Jmp.pp;
      do_if blk_t ~f:(fun blk ->
          fprintf ppf "  %a:@\n" Tid.pp (Term.tid blk));
      do_if sub_t ~f:(fun sub ->
          fprintf ppf "%s:@\n" (Sub.name sub)))

let pp_binding ppf (v,r) =
    fprintf ppf "  %a -> %s" Var.pp v @@ match Bil.Result.value r with
    | Bil.Bot -> "undefined"
    | Bil.Imm w -> Word.string_of_value w
    | Bil.Mem m -> "<mem>"
let pp_bindings ppf ctxt = Seq.pp pp_binding ppf ctxt#bindings

let main proj =
  let prog = Project.program proj in
  let init = new Context.t ~argv:[|"name"; "hello"; "world"|] proj in
  let interp = new Interpreter.t in
  Term.enum sub_t prog |> Seq.find ~f:(fun sub ->
      Sub.name sub = "_start") |> function
  | None -> invalid_arg "run: no main function"
  | Some main -> match Main.run (interp#eval sub_t main) init with
    | (Ok (),ctxt) ->
      info "evaluation finished after %d steps at term: %a"
        (List.length ctxt#trace) Tid.pp ctxt#current;
      let result = Var.create "main_result" reg32_t in
      let () = match ctxt#lookup result with
        | None -> warning "result is unknown";
        | Some r -> match Bil.Result.value r with
          | Bil.Bot -> warning "result is undefined";
          | Bil.Imm w -> info "result is %a" Word.pp w;
          | Bil.Mem _ -> warning "result is unsound" in
      debug "CPU State:@\n%a@\n" pp_bindings ctxt;
      debug "Backtrace:@\n@[<v>%a@]@\n" pp_backtrace ctxt;
      ctxt#project;
    | (Error err,ctxt) ->
      debug "program failed with %s\n" (Error.to_string err);
      debug "Backtrace:@\n%a@\n" pp_backtrace ctxt;
      ctxt#project

let () = Project.register_pass main
