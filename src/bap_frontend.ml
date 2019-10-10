let man = {|
# DESCRIPTION

The main command line utility of the Binary Analysis platform used to
invoke BAP analysis from the command line. The utility loads all
plugins, parses command line arguments, and runs the specified
command. The default command is $(b,disassemble), which disassembles
and analyzes the specified file.

# PLUGINS

The BAP frontend does nothing by itself and all the functionality
comes from various BAP plugins. There are many extension points in
BAP, but writing a disassembler pass is probably the easiest to start
with. See $(bap, dis --help) for more details.

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

let is_option = String.is_prefix ~prefix:"-"

(* To preserve backward compatibility and enhance usability we
   automatically add the `disassemble' command if the first argument is
   a file. However, since we can have a potential clash between a
   command name, a command name could be prefixed with `:`, so that
   it will be interpreter literally as a keyword, not as a file.
*)
let autocorrect_input args =
  Array.of_list @@ match Array.to_list args with
  | [] | [_] as args -> args
  | name :: arg :: args as input ->
    if is_option arg then input else match qualified arg with
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
    pp_info (commands ctxt)
    pp_info (plugins ctxt)


let () =
  Command.declare ~doc:"does nothing" "." Command.args @@
  fun _ -> Ok ()


let () =
  let what = Command.argument @@ Type.enum [
      "plugins", `Plugins;
      "commands", `Commands;
      "recipes", `Recipes;
      "tags", `Tags;
      "features", `Tags;
      "passes", `Passes;
      "formats", `Formats;
    ] in
  let filter = Command.parameter "filter" Type.(list string) in
  let doc = "explores various BAP facilities" in
  let push x xs = match xs with
    | None -> Some [x]
    | Some xs -> Some (x::xs) in
  let parse_filter = List.fold ~init:(None,None)
      ~f:(fun (features,exclude) elt ->
          match String.chop_prefix elt ~prefix:"-" with
          | Some elt -> (features, push elt exclude)
          | None ->
            let elt =
              String.chop_prefix elt ~prefix:"+" |>
              Option.value ~default:elt in
            (push elt features, exclude)) in
  Command.declare ~doc "list" Command.(args $what $filter) @@
  fun what filter ctxt ->
  let features, exclude = parse_filter filter in
  match what with
  | `Recipes ->
    Format.printf "Use the `print-recipes' command to list recipes, e.g.,
      bap print-recipes@\n%!";
    Ok ()
  | `Plugins ->
    Format.printf "%a%!" pp_info (Context.plugins ?features ?exclude ctxt);
    Ok ()
  | `Commands ->
    Format.printf "%a%!" pp_info
      (Context.commands ?features ?exclude ctxt);
    Ok ()
  | `Passes ->
    Project.passes () |>
    List.iter ~f:(fun p -> Format.printf "  %s@\n%!" (Project.Pass.name p));
    Ok ()
  | `Tags ->
    Context.features ctxt |>
    List.iter ~f:(fun feature ->
        let plugins =
          Context.plugins ~features:[feature] ctxt |>
          List.map ~f:Context.name in
        let pp_sep ppf () = Format.fprintf ppf ",@ " in
        Format.printf "  %-24s @[<hov>%a@]@\n%!"
          feature Format.(pp_print_list ~pp_sep pp_print_string) plugins);
    Ok ()
  | `Formats ->
    Project.available_writers () |>
    List.iter ~f:(fun (name,`Ver ver, desc) ->
        let name = sprintf "%s (%s)" name ver in
        let desc =
          Option.value desc ~default:"no description provided" in
        Format.printf "%-24s %s@\n%!" name desc);
    Ok ()

let () =
  let _unused : (module unit) = (module Bap.Std) in
  let argv = autocorrect_input Sys.argv in
  match Bap_main.init ~default:print_info ~name:"bap" ~man ~argv () with
  | Ok () -> ()
  | Error (Error.Exit_requested code) -> exit code
  | Error Error.Configuration -> exit 1
  | Error err -> Format.eprintf "%a@\n%!" Error.pp err;
    exit 1
