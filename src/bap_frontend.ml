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
with. See $(b,bap dis --help) for more details.

# BUGS

Report bugs to
  https://github.com/BinaryAnalysisPlatform/bap/issues

# SEE ALSO

$(b,bapbundle)(1), $(b,bapbuild)(1), $(b,bap)(3)
|}
open Bap.Std
open Core_kernel
open Regular.Std
open Bap_main.Extension

module type unit = sig end


let pp_info ppf infos =
  List.iter infos ~f:(fun info ->
      Format.fprintf ppf "  %-24s %s@\n"
        (Configuration.info_name info)
        (Configuration.info_doc info))

let commands ctxt = Configuration.commands ctxt
let plugins ctxt = Configuration.plugins ctxt

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

If no command line options or arguments are specified, then this
message is printed. To hush this message specify the `.' command,
e.g., `bap .'. The command could be omitted altogether, in that case the
`disassemble' command is assumed, e.g., `bap /bin/ls' is the same
as `bap disassemble /bin/ls'. Not that the default command is
subject to change, so it is better not to rely on this behavior in
your automation tools.

Run 'bap <COMMAND> --help' for more information a command.
Run 'bap --<PLUGIN>-help for more information about a plugin.
Run 'bap --help' for the detailed manual.
|}
    pp_info (commands ctxt)
    pp_info (plugins ctxt)


let () =
  let doc = {|
# DESCRIPTION

Does nothing and prints nothing. This command could be used to hush
the default bap usage message, which is printed when the bap command
is issued without any arguments.
|} in
  Command.declare ~doc "." Command.args @@
  fun _ -> Ok ()


let unqualified str = match String.rindex str '.' with
  | None -> str
  | Some p -> String.subo ~pos:(p+1) str

let matches default set str = match set with
  | None -> default
  | Some set ->
    Set.mem set str ||
    let str = unqualified str in
    Set.mem set str ||
    Set.mem set (String.lowercase str)

let strings = Option.map ~f:(Set.of_list (module String))

let pp_sep ppf () = Format.fprintf ppf ",@ "
let pp_strings ppf xs =
  Format.(pp_print_list ~pp_sep pp_print_string) ppf xs

type entity = [
  | `Entities
  | `Plugins
  | `Commands
  | `Recipes
  | `Tags
  | `Passes
  | `Formats
  | `Classes
  | `Theories
  | `Agents
]

let entities : (string, entity) List.Assoc.t = [
  "entities", `Entities;
  "plugins", `Plugins;
  "commands", `Commands;
  "recipes", `Recipes;
  "tags", `Tags;
  "features", `Tags;
  "passes", `Passes;
  "formats", `Formats;
  "classes", `Classes;
  "theories", `Theories;
  "agents", `Agents;
]

let entity_name = function
  | `Entities -> "entities"
  | `Plugins -> "plugins"
  | `Commands -> "commands"
  | `Recipes -> "recipes"
  | `Tags -> "features"
  | `Passes -> "passes"
  | `Formats -> "formats"
  | `Classes -> "classes"
  | `Theories -> "theories"
  | `Agents -> "agents"

let entity_desc : (entity * string) list = [
  `Entities, "prints this message";
  `Plugins, "installed BAP plugins";
  `Commands, "bap utility commands";
  `Recipes, "installed recipes";
  `Tags, "plugin capabilities";
  `Passes, "disassembler passes";
  `Formats, "data output formats";
  `Classes, "knowledge representation classes";
  `Theories, "installed theories";
  `Agents, "knowledge providers";
]

let () =
  let what = Command.argument @@ Type.enum entities in
  let filter = Command.parameter Type.(list string) "filter"
      ~aliases:["f"]
      ~doc:"Select what to print.
      The input is a comma-separated list of names, optionally
      prefixed with the minus sign, e.g., $(b,--filter=foo,-bar).
      When specified, then only entities with specified names will
      be printed. If the name is specified with the $(b,-) before it,
      then entities with that name will not be printed." in
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
  let requires, exclude = parse_filter filter in
  let ctxt = Configuration.refine ?provides:requires ?exclude ctxt in
  let requested = strings requires
  and excluded = strings exclude in
  let selected name =
    matches true requested name && not (matches false excluded name) in
  match what with
  | `Recipes ->
    ignore (Sys.command "bap print-recipes");
    Format.printf "Use the `print-recipes' for the detailed list of recipes\n";
    Ok ()
  | `Plugins ->
    Format.printf "%a%!" pp_info (Configuration.plugins ctxt);
    Ok ()
  | `Commands ->
    Format.printf "%a%!" pp_info (Configuration.commands ctxt);
    Ok ()
  | `Passes ->
    Project.passes () |>
    List.iter ~f:(fun p -> Format.printf "  %s@\n%!" (Project.Pass.name p));
    Ok ()
  | `Tags ->
    Configuration.features ctxt |>
    List.iter ~f:(fun feature ->
        let ctxt = Configuration.refine ~provides:[feature] ctxt in
        let plugins = Configuration.plugins  ctxt |>
                      List.map ~f:Configuration.info_name in
        Format.printf "  %-24s @[<hov>%a@]@\n%!" feature pp_strings plugins);
    Ok ()
  | `Formats ->
    Data.all_writers () |>
    List.iter ~f:(fun (typename,writers) ->
        if selected typename then begin
          Format.printf "  %s:@\n" typename;
          List.iter writers ~f:(fun (name,`Ver ver, desc) ->
              let name = sprintf "%s (%s)" name ver in
              let desc =
                Option.value desc ~default:"no description provided" in
              Format.printf "    %-22s %s@\n%!" name desc)
        end);
    Ok ()
  | `Classes ->
    let module Name = Bap_knowledge.Knowledge.Name in
    let open Bap_knowledge.Knowledge.Documentation in
    classes () |>
    List.iter ~f:(fun (cls,properties) ->
        let name = Name.show (Class.name cls)
        and desc = Class.desc cls in
        if selected name
        then begin
          Format.printf "@\n  %-30s @[<hov>%s@]@\n" name desc;
          List.iter properties ~f:(fun prop ->
              let name = Name.show (Property.name prop)
              and desc = Property.desc prop in
              Format.printf "    - %-26s @[<hov>%s@]@\n%!" name desc)
        end);
    Ok ()
  | `Theories ->
    let open Bap_core_theory.Theory.Documentation in
    let module Name = Bap_knowledge.Knowledge.Name in
    theories () |> List.iter ~f:(fun theory ->
        let name = Name.show (Theory.name theory)
        and desc = Theory.desc theory
        and requires = Theory.requires theory
        and provides = Theory.provides theory in
        if selected name then begin
          Format.printf "  %-24s @[<hov>%a@]@\n" name
            Format.pp_print_text desc;
          Format.printf "    %-22s @[<hov>%a@]@\n" "requires:"
            pp_strings requires;
          Format.printf "    %-22s @[<hov>%a@]@\n@\n%!" "provides:"
            pp_strings provides;
        end);
    Ok ()
  | `Agents ->
    let open Bap_knowledge.Knowledge.Documentation in
    let module Name = Bap_knowledge.Knowledge.Name in
    agents () |> List.iter ~f:(fun agent ->
        let name = Name.show (Agent.name agent)
        and desc = Agent.desc agent in
        if selected name
        then Format.printf "  %-32s @[<hov>%a@]@\n" name
            Format.pp_print_text desc;
        ());
    Ok ()
  | `Entities ->
    List.iter entity_desc ~f:(fun (entity,desc) ->
        let name = entity_name entity in
        if selected name
        then Format.printf "  %-24s @[<hov>%a@]@\n" name
            Format.pp_print_text desc);
    Ok ()

let () =
  let _unused : (module unit) = (module Bap.Std) in
  let () =
    try if String.(Sys.getenv "BAP_DEBUG" <> "0") then
        Printexc.record_backtrace true
    with Caml.Not_found -> () in
  Sys.(set_signal sigint (Signal_handle exit));
  at_exit Format.(pp_print_flush err_formatter);
  match Bap_main.init ~default:print_info
          ~default_command:"disassemble"
          ~name:"bap" ~man ~argv:Sys.argv ()
  with
  | Ok () -> ()
  | Error (Error.Exit_requested code) -> exit code
  | Error Error.Configuration -> exit 1
  | Error err -> Format.eprintf "%a@\n%!" Error.pp err;
    exit 1
