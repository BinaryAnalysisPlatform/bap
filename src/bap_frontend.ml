let man = {|
# DESCRIPTION

The main command line utility of the Binary Analysis platform used to
invoke BAP analysis from the command line. The utility loads all
plugins, parses command line arguments, and runs the specified
command. The default command is $(b,disassemble), which disassembles
and analyzes the specified file.

# PLUGINS

The BAP frontend does nothing by itself and all the functionality
comes from various BAP plugins. BAP provides various extension points,
but a disassembler pass is a good start. See $(bap, dis --help) for
more information.

# BUGS

Report bugs to
  https://github.com/BinaryAnalysisPlatform/bap/issues

# SEE ALSO

$(b,bapbundle)(1), $(b,bapbuild)(1), $(b,bap)(3)"

|}
open Bap.Std
open Core_kernel
open Bap_main.Extension

module type unit = sig end

let qualified cmd =
  if String.length cmd > 2 && Char.equal cmd.[0] ':'
  then Some (String.subo ~pos:1 cmd)
  else None

(* To preserve backward compatibility and enhance usability we
   automatically add the `disassemble' command if the first argument is
   a file. However, since we can have a potential clash between a
   command name, a command name could be prefixed with `:`, so that
   it will be interpreter literally as a keyword, not as a file.
*)
let autocorrect_input args =
  Array.of_list @@ match Array.to_list args with
  | [] | [_] as args -> args
  | name :: arg :: args -> match qualified arg with
    | Some cmd -> name::cmd::args
    | None ->
      if Sys.file_exists arg && not (Sys.is_directory arg)
      then name :: "disassemble" :: arg :: args
      else name :: arg :: args

let pp_info ppf infos =
  List.iter infos ~f:(fun info ->
      Format.fprintf ppf "  %-24s %s@\n"
        (Context.name info)
        (Context.doc info))

let admin_commands ctxt = Context.commands ctxt
let commands ctxt = Context.commands ctxt
let plugins ctxt = Context.plugins ctxt


let print_info ctxt =
  Result.return @@
  Format.printf {|
Usage:
  bap <COMMAND> [<OPTIONS>]

Common options:
  --version                prints the version number and exits;
  --plugin-path <DIR>      adds <DIR> to the plugins search path;
  --logdir <DIR>           creates a log file in <DIR>;
  --recipe <VAL>           extracts command line arguments from the recipe.

Management commands:
%a

Commands:
%a

Plugins:
%a

If no command is specified and the first parameter is a file,
then the `disassemble' command is assumed. If the name of a
command conflicts with an existing file, prefix the name with
`:', e.g., `bap :plugins'.

Run 'bap <COMMAND> --help' for more information a command.
Run 'bap --<PLUGIN>-help for more information about a plugin.
Run 'bap --help' for the detailed manual.
|}
    pp_info (admin_commands ctxt)
    pp_info (commands ctxt)
    pp_info (plugins ctxt)


let () =
  Command.declare ~doc:"does nothing" "." Command.args @@
  fun _ -> Ok ()

let () =
  let _unused : (module unit) = (module Bap.Std) in
  let argv = autocorrect_input Sys.argv in
  match Bap_main.init ~default:print_info ~name:"bap" ~man ~argv () with
  | Ok () -> ()
  | Error (Error.Exit_requested code) -> exit code
  | Error err -> Format.eprintf "%a@\n%!" Error.pp err;
    exit 1
