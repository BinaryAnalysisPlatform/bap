open Core_kernel.Std
open Bap.Std
open Primus.Std
open Monads.Std
include Self()
open Format

type value = Primus.Generator.t

type parameters = {
 argv  : string list;
 envp  : (string * string) list;
 entry : string option;
 memory : (addr * value) list;
 variables : (string * value) list;
}


module Param = struct
  open Sexp
  type t = parameters

  let int = int_of_string

  let parse_value = function
    | List [Atom x; Atom y; Atom z] ->
      Primus.Generator.Random.lcg ~min:(int x) ~max:(int y) (int z)
    | List [Atom x; Atom y] ->
      Primus.Generator.Random.Seeded.lcg ~min:(int x) ~max:(int y) ()
    | Atom x -> Primus.Generator.static (int x)
    | _ -> invalid_arg "Parse error:
     expected 'value := const | (min max) | (min max seed)'"

  open Config;;

  manpage [
    `S "DESCRIPTION";
    `P "Run program in the Primus emulator. ";
  ];;

  let argv = param (array string)  "argv"
      ~doc:"Program command line arguments";;

  let envp = param (array string) "env"
      ~doc:"Program environemt as a list of VAR=VAL pairs";;

  let entry = param (some string) "entry-point"
      ~doc: "When specified, then start the execution from $(docv),
      otherwise the execution will be started from a default entry point";;

end

module Machine = Primus.Machine.Make(Monad.Ident)
module Main = Primus.Machine.Main(Machine)
module Interpreter = Primus.Interpreter.Make(Machine)
module Environment(Machine : Primus.Machine.S) = struct
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

let main {Config.get=(!)} proj =
  let open Param in
  let prog = Project.program proj in
  let init = new Primus.Context.t ~envp:!envp ~argv:!argv proj in
  let interp = new Interpreter.t in
  let subs = Term.enum sub_t prog in
  let is_entry_point = match !entry with
    | Some name -> fun sub -> Sub.name sub = name
    | None -> fun sub -> Term.has_attr sub Sub.entry_point in
  let entry = match Seq.find subs ~f:is_entry_point with
    | Some entry -> entry
    | None when Option.is_some !entry ->
      invalid_arg "can't find the specified entry point"
    | None ->
      Seq.find subs ~f:(fun sub -> not (Term.has_attr sub Sub.stub)) |>
      function
      | Some entry -> entry
      | None -> invalid_arg "unable to find a suitable entry point" in
  match Main.run (interp#eval sub_t entry) init with
  | (Ok (),ctxt) ->
    info "evaluation finished after %d steps at term: %a"
      (List.length ctxt#trace) Tid.pp ctxt#current;
    debug "Backtrace:@\n%a@\n" pp_backtrace ctxt;
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
    error "program failed with %s\n" (Primus.Error.to_string err);
    info "Backtrace:@\n%a@\n" pp_backtrace ctxt;
    ctxt#project

let () =
  Config.when_ready (fun conf ->
      Project.register_pass (main conf))
