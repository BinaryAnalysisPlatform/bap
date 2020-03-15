let doc = {|
# DESCRIPTION

Loads Primus systems and registers them in the system repository.
The systems are searched in the following directories:

  - the current working directory;

  - the directories specified by $(b,--primus-system-add-path);

  - the directory specified at the configuration time, usually
    (\$share/primus)

The directories are searched and all files that have the [.asd]
extension are loaded. Each file may define more than one system and
reference other systems (via the [depends-on]) no matter whether they
are loaded before or after (systems and components are resolved when
the system is run). Each system should have a distinctive name and if
there are systems with repetetive names an exception is raised.

The system description file has the following grammar:

```
   systems ::= <system-definition> ...
   system-definition ::= (defsystem <ident> <option> ...)
   option ::=
     | :description <string>
     | :components (<ident> ...)
     | :depends-on (<ident> ...)
   ident ::= <string> | <string>:<string>

The $(b,:components) option is the list of components that comprise the
system. And the $(b, :depends-on) option specifies the list of
systems on which the defined system depends. The components of those
systems are essentially copied to the defined system.

This plugin also provides introspection commands $(b,primus-systems),
$(b,primus-components), and $(b,primus-observations) that provides the
lists of currently known systems, components, and observations. See
corresponding command help pages for more information.
```
|}
open Bap_knowledge
open Core_kernel
open Bap_main
open Bap_primus.Std
open Format

let command what =
  sprintf "
# DESCRIPTION

Prints a list of Primus %ss. If an argument is specified,
then prints the detailed information about the %s with
that name. If several arguments are specified then prints
information (not detailed) about only those %ss that have
the specified names" what what what


let sys_path = Primus_systems_config.path
let paths = Extension.Configuration.parameters
    Extension.Type.(list dir) "add-path"
    ~doc:"adds the path to the list of paths where Primus systems \
          are searched"

let provides = [
  "primus";
  "primus-systems";
]

let () = Extension.declare ~provides ~doc @@ fun ctxt ->
  let usr_paths = Extension.Configuration.get ctxt paths |>
                  List.concat in
  [Filename.current_dir_name] @ usr_paths @ [sys_path] |>
  List.iter ~f:(fun path ->
      Sys.readdir path |>
      Array.iter ~f:(fun file ->
          if String.is_suffix file ~suffix:".asd" then
            let path = Filename.concat path file in
            match Primus.System.from_file path with
            | Error failed ->
              eprintf "Failed to parse system %s: %a@\n%!"
                file Primus.System.pp_parse_error failed
            | Ok systems ->
              List.iter systems ~f:(Primus.System.Repository.add)));
  Ok ()

let names =
  Extension.(Command.argument Type.(list string))

let make_info_command list name =
  let doc = command name in
  let name = sprintf "primus-%ss" name in
  Extension.Command.(declare ~doc name (args $ names)) @@ fun names _ctxt ->
  let detailed = match names with [_] -> true | _ -> false in
  let names = List.map names Knowledge.Name.read |>
              Set.of_list (module Knowledge.Name) in
  let selected info =
    Set.is_empty names || Set.mem names (Primus.Info.name info) in
  list () |> List.iter ~f:(fun info ->
      if selected info then begin
        Format.printf "%a" Primus.Info.pp info;
        if detailed
        then Format.printf "%s@\n" (Primus.Info.long info);
      end);
  Ok ()


let () =
  make_info_command Primus.System.Repository.list "system";
  make_info_command Primus.Components.list "component";
  make_info_command Primus.Observation.list "observation"
