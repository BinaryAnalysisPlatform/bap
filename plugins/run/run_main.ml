open Core_kernel.Std
open Bap.Std
open Primus.Std
open Monads.Std

open Format

module Mach = Machine.Make(Monad.Ident)
module Main = Machine.Main(Mach)
module Interpreter = Interpreter.Make(Mach)
module Linker = Linker.Make(Mach)

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
  let init = new Context.t proj in
  let interp = new Interpreter.t in
  Term.enum sub_t prog |> Seq.find ~f:(fun sub ->
      Sub.name sub = "main") |> function
  | None -> invalid_arg "run: no main function"
  | Some main -> match Main.run (interp#eval sub_t main) init with
    | (Ok (),ctxt) ->
      printf "evaluation finished after %d steps at term: %a\n"
        (List.length ctxt#trace) Tid.pp ctxt#current;
      let result = Var.create "main_result" reg32_t in
      let () = match ctxt#lookup result with
        | None -> printf "result is unknown\n";
        | Some r -> match Bil.Result.value r with
          | Bil.Bot -> printf "result is undefined\n";
          | Bil.Imm w -> printf "result is %a\n" Word.pp w;
          | Bil.Mem _ -> printf "result is unsound\n" in
      printf "CPU State:@\n%a@\n" pp_bindings ctxt;
      printf "Backtrace:@\n@[<v>%a@]@\n" pp_backtrace ctxt;
      ctxt#project;
    | (Error err,ctxt) ->
      printf "program failed with %s\n" (Error.to_string err);
      printf "Backtrace:@\n%a@\n" pp_backtrace ctxt;
      ctxt#project

let () = Project.register_pass main
