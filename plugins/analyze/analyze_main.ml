let doc = {|
# DESCRIPTION

Analyses the knowledge base. Loads the knowledge base and executes the
specified commands or run the REPL if no commands or script files were
specified.

The knowledge base is not saved until the $(b,save) directive is
issued. The knowledge base itself is optional and can be specified
using the $(b,--project) parameter, which defaults to $(b,a.proj).

The commands can be entered via the REPL, which features completion
(hit the $(b,<TAB>) key) and contextual hints. Alternatively, commands
could be specified via a script file (see $(b,--script)), with one
command per line, or using the command-line itself, with each command
delimited with quotes, e.g.,

```
    bap analyze commands # prints all commands
    bap analyze --project=test.proj 'subroutines :unit file:echo'
```

All commands are stored in the history file that persists between
invocations of bap. The location of the file could be specified with
the $(b,--history) option, which could also be used to interactively
create the script files.

# GRAMMAR

The expected input is a list of commands or directives separated with
the newline characters. The directive $(b,directive) will output the
list of available directives and corresponding descriptions. The
$(b,commands) directive will list all available commands, and $(b,help
<command>) will provide detailed information about $(b,<command>) its
syntax and semantics.

The syntax rules for commands are described in the
$(b,Project.Analysis) API documentation but in general it follows the
common command line syntax, i.e., each command is a sequence of words
separated by whitespaces, with keyworded arguments prefixed with
$(b,:) instead of $(b,-) or $(b,--), e.g.,

```
     subroutines :unit file:echo :matches malloc
```
|}


open Core_kernel
open Bap_main
open Bap_knowledge
open Bap.Std

include Loggers()

type problem =
  | Unknown_command of {package : string; name : string}
  | Conflict of Knowledge.Conflict.t * string * string list
  | Bad_directive of {dir : string; msg : string}
  | Sys_error_on_load of exn

type Extension.Error.t += Fail of problem

type ctxt = {
  path : string;
  package : string;
  history : string;
  quit : bool;
  commands : Project.Analysis.info Map.M(Knowledge.Name).t;
  directives : string Map.M(String).t;
}


let collect_commands () =
  Project.Analysis.registered () |>
  List.fold ~init:(Map.empty (module Knowledge.Name))
    ~f:(fun hints info ->
        let name = Project.Analysis.name info in
        Map.add_exn hints name info)


let initial_ctxt ~history directives path = {
  path;
  package = "user";
  quit = false;
  commands = collect_commands ();
  history;
  directives = List.fold directives
      ~init:String.Map.empty
      ~f:(fun dirs (name,_,desc) ->
          Map.add_exn dirs name desc)
}

let fail problem = Error (Fail problem)

let break_line str =
  String.split ~on:' ' str |>
  List.filter ~f:(fun s -> not (String.is_empty s))

let exec_command ?(package="bap") cmd args =
  match Project.Analysis.find ~package cmd with
  | None -> fail (Unknown_command {package; name=cmd})
  | Some analysis ->
    let code = Project.Analysis.apply analysis args in
    match Toplevel.try_exec code with
    | Ok () -> Ok ()
    | Error err -> fail (Conflict (err,cmd,args))

let in_package ctxt package =
  Toplevel.exec @@ Knowledge.Symbol.set_package package;
  {ctxt with package}

let set_package args ctxt = match args with
  | [pkg] -> Ok (in_package ctxt pkg)
  | _ -> fail (Bad_directive {
      dir = "in-package";
      msg = "expects exactly one argument";
    })


let save_base args ctxt = match args with
  | [] | [_] ->
    let path = Option.value (List.hd args)
        ~default:ctxt.path in
    Knowledge.save (Toplevel.current ()) path;
    Ok ctxt
  | _ -> fail (Bad_directive {
      dir = "save";
      msg = "takes at most one argument";
    })

let no_args dir f args ctxt = match args with
  | [] -> f ctxt
  | _ -> fail (Bad_directive {dir; msg = "doesn't accept arguments"})

let quit = no_args "quit" @@ fun ctxt -> Ok {ctxt with quit = true}
let clear = no_args "clear" @@ fun ctxt ->
  LNoise.clear_screen ();
  Ok ctxt

let short str =
  let len = min
      (String.length str)
      (Option.value (String.index str '.') ~default:40) in
  String.lowercase @@ String.subo str ~len

let list_commands = no_args "commands" @@ fun ctxt ->
  Project.Analysis.registered () |>
  List.iter ~f:(fun analysis ->
      let name = Project.Analysis.name analysis
      and desc = short @@ Project.Analysis.desc analysis in
      Format.printf "%-40s %s@\n%!" (Knowledge.Name.to_string name) desc);
  Ok ctxt

let help_msg = "\
Type `commands' for the list of available commands or `directives' \
for the list of directives. Commands, unlike directives, are properly \
namespaced (packaged), the name of the currently opened namesapce \
(package) is displayed in the prompt. Most of the commands accept \
required or optional arguments. Some arguments are keyworded, i.e., \
them must be prefixed with the specified keyword, e.g.,

instruction /bin/ls:0x8080 :semantics bil

Type `help <command> ...` for the detailed description of the \
<command>. If more than one command is specified, then the detailed \
description will be printed for each.
"

let help args ctxt = match args with
  | [] ->
    Format.printf "@[%a@]" Format.pp_print_text help_msg;
    Ok ctxt
  | _ :: _ :: _ ->
    fail (Bad_directive {
        dir = "help";
        msg = "expects zero or one argument";
      })
  | [cmd] ->
    match Map.find ctxt.directives cmd with
    | Some desc -> Format.printf "%s@\n" desc; Ok ctxt
    | None ->
      let name = Knowledge.Name.read ~package:ctxt.package cmd in
      match Map.find ctxt.commands name with
      | None -> fail (Bad_directive {
          dir = "help";
          msg = sprintf "no such command %s in package %s"
              cmd ctxt.package;
        })
      | Some info ->
        let grammar = Project.Analysis.grammar info
        and desc = Project.Analysis.desc info  in
        Format.printf "SYNOPSIS\n\n%s %s\n\nDESCRIPTION\n@[%a@]\n"
          cmd (Project.Analysis.Grammar.to_string grammar)
          Format.pp_print_text desc;
        Ok ctxt

let list_directives = no_args "directives" @@ fun ctxt ->
  Map.iteri ctxt.directives ~f:(fun ~key:dir ~data:desc ->
      Format.printf "%-40s %s@\n%!" dir desc);
  Ok ctxt

let directives = [
  "in-package", set_package, "sets the default package";
  "save", save_base, "saves the knowledge base on disk";
  "quit", quit, "quits the interactive session";
  "clear", clear, "clears the screen";
  "commands", list_commands, "lists known commands";
  "help", help,  "prints the help message";
  "directives", list_directives, "lists directives";
]

let find_directive dir = List.find_map directives ~f:(fun (s,f,_) ->
    if String.equal s dir then (Some f) else None)

let dispatch str ctxt = match break_line str with
  | [] -> Ok ctxt
  | cmd :: args -> match find_directive cmd with
    | Some dir -> dir args ctxt
    | None ->
      match exec_command ~package:ctxt.package cmd args with
      | Ok () -> Ok ctxt
      | Error err -> Error err

let report_error err =
  Format.eprintf "%a@\n%!" Extension.Error.pp err

let load_history filename =
  if Sys.file_exists filename
  then match LNoise.history_load ~filename with
    | Ok () -> ()
    | Error msg ->
      Format.eprintf "Failed to load the history file: %s@\n%!" msg

let remember ctxt input =
  match LNoise.history_add input with
  | Error msg -> warning "failed to remember the input: %s" msg
  | Ok () ->
    match LNoise.history_save ~filename:ctxt.history with
    | Ok () -> ()
    | Error msg -> warning "failed to write history: %s" msg

let complete ctxt prefix completions =
  let matches = String.is_prefix ~prefix in
  List.iter directives ~f:(fun (name,_,_) ->
      if matches name
      then LNoise.add_completion completions name);
  Project.Analysis.registered () |>
  List.iter ~f:(fun info ->
      let name = Project.Analysis.name info in
      let pkg = Knowledge.Name.package name in
      let short = Knowledge.Name.unqualified name in
      let full = Knowledge.Name.to_string name in
      if pkg = ctxt.package && matches short
      then LNoise.add_completion completions short
      else if matches full || matches short
      then LNoise.add_completion completions full)


let hint {package; commands} prefix =
  match break_line prefix with
  | [] -> None
  | cmd :: args ->
    let inputed = List.length args in
    let cmd = Knowledge.Name.read ~package cmd in
    match Map.find commands cmd with
    | None -> None
    | Some info ->
      let grammar = break_line @@
        Project.Analysis.Grammar.to_string @@
        Project.Analysis.grammar info in
      match List.drop grammar inputed with
      | [] | [""] -> None
      | xs ->
        Option.return (" " ^ String.concat ~sep:" " xs,
                       LNoise.Yellow,
                       false)

let rec interactive_loop ctxt =
  Format.printf "%!";
  LNoise.set_completion_callback (complete ctxt);
  LNoise.set_hints_callback (hint ctxt);
  match LNoise.linenoise (ctxt.package ^ "> ") with
  | exception Sys.Break -> interactive_loop ctxt
  | None -> Ok ()
  | Some input ->
    remember ctxt input;
    match dispatch input ctxt with
    | Error err -> report_error err; interactive_loop ctxt
    | Ok {quit=true} -> Ok ()
    | Ok ctxt -> interactive_loop ctxt

let load_script = function
  | None -> Ok []
  | Some path ->
    try Ok (In_channel.read_lines path)
    with exn -> fail (Sys_error_on_load exn)


let dispatch_commands commands ctxt =
  let open Result.Monad_infix in
  List.fold commands ~init:(Ok ctxt) ~f:(fun ctxt cmd ->
      ctxt >>= dispatch cmd)

let run_non_interactive commands script ctxt =
  let open Result.Monad_infix in
  dispatch_commands commands ctxt >>= fun ctxt ->
  load_script script >>= fun commands ->
  dispatch_commands commands ctxt

let string_of_problem = function
  | Unknown_command {package; name} ->
    sprintf "Can't find a command %S in the package %S" name package
  | Conflict (err,cmd,_) ->
    sprintf "Command %s failed with %s" cmd
      (Knowledge.Conflict.to_string err)
  | Bad_directive {msg} -> sprintf "Error: %s" msg
  | Sys_error_on_load exn ->
    sprintf "Failed to load a script: %s" (Exn.to_string exn)


let () =
  let open Extension in
  let open Extension.Command in
  let knowledge = parameter
      Type.("knowledge-base" %: string =? "a.proj")
      "project" ~aliases:["k"]
      ~doc:"The path to a knowledge base." in
  let commands = arguments Extension.Type.("command" %: string)
      ~doc:"The command to execute." in
  let script = parameter Type.("path" %: some file) "script"
      ~aliases:["s"]
      ~doc:"The path to a script file with commands." in
  let history =
    let history_location =
      match Sys.getenv_opt "HOME" with
      | None | Some "" -> ".bap_history"
      | Some home -> Filename.concat home ".bap_history" in
    parameter Type.("path" %: file =? history_location) "history"
      ~aliases:["H"] in
  declare "analyze" ~doc (args $knowledge $commands $script $history) @@
  fun base commands script history _ctxt ->
  Analyze_core_commands.register ();
  if Sys.file_exists base
  then Toplevel.set @@ Knowledge.load base;
  let ctxt = in_package (initial_ctxt history directives base) "bap" in
  match commands,script with
  | [], None ->
    load_history history;
    interactive_loop ctxt
  | _ -> match run_non_interactive commands script ctxt with
    | Ok _ -> Ok ()
    | Error err -> Error err

let () = Extension.Error.register_printer @@ function
  | Fail problem -> Some (string_of_problem problem)
  | _ -> None
