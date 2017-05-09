open Core_kernel.Std
open Bap.Std
open Bap_primus.Std
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
  open Config;;

  manpage [
    `S "DESCRIPTION";
    `P "Run a program in the Primus emulator. ";
  ];;

  let argv = param (array string)  "argv"
      ~doc:"Program command line arguments";;

  let envp = param (array string) "env"
      ~doc:"Program environemt as a comma separated list of VAR=VAL pairs";;

  let entry = param string "entry-point"
      ~default:"_start"
      ~doc: "When specified, start the execution from $(docv)";;
end

module Machine = Primus.Machine.Make(Monad.Ident)
module Main = Primus.Machine.Main(Machine)
module Interpreter = Primus.Interpreter.Make(Machine)
module Linker = Primus.Linker.Make(Machine)

let pp_backtrace ppf ctxt =
  let prog = ctxt#program in
  fprintf ppf "Traceback (most recent instruction last):@\n@[<v>";
  List.iter (List.rev ctxt#trace) ~f:(fun tid ->
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
          fprintf ppf "%s:@\n" (Sub.name sub)));
  fprintf ppf "@]@\n"

let pp_binding ppf (v,r) =
    fprintf ppf "  %a -> %s" Var.pp v @@ match Bil.Result.value r with
    | Bil.Bot -> "undefined"
    | Bil.Imm w -> Word.string_of_value w
    | Bil.Mem m -> "<mem>"
let pp_bindings ppf ctxt = Seq.pp pp_binding ppf ctxt#bindings

let name_of_entry arch entry =
  let width = Arch.addr_size arch |> Size.in_bits in
  if String.is_empty entry
  then invalid_arg "Entry point should be a non-empty string"
  else match entry.[0] with
    | '%' -> `tid (Tid.from_string_exn entry)
    | '0' -> `addr (Addr.of_int64 ~width (Int64.of_string entry))
    | _ -> `symbol entry

let main {Config.get=(!)} proj =
  let open Param in
  let init = new Primus.Context.t ~envp:!envp ~argv:!argv proj in
  let interp = new Interpreter.t in
  let entry_point = name_of_entry (Project.arch proj) !entry in
  match Main.run (Linker.exec entry_point interp) init with
  | (Ok (),ctxt) ->
    info "evaluation finished after %d steps at term: %a"
      (List.length ctxt#trace) Tid.pp ctxt#current;
    debug "%a" pp_backtrace ctxt;
    let result = Var.create "main_result" reg32_t in
    let () = match ctxt#lookup result with
      | None -> warning "result is unknown";
      | Some r -> match Bil.Result.value r with
        | Bil.Bot -> warning "result is undefined";
        | Bil.Imm w -> info "result is %a" Word.pp w;
        | Bil.Mem _ -> warning "result is unsound" in
    debug "CPU State:@\n%a@\n" pp_bindings ctxt;
    debug "%a" pp_backtrace ctxt;
    ctxt#project;
  | (Error err,ctxt) ->
    error "program failed with %s\n" (Primus.Error.to_string err);
    error "%a" pp_backtrace ctxt;
    ctxt#project

let () =
  Config.when_ready (fun conf ->
      Project.register_pass (main conf))
