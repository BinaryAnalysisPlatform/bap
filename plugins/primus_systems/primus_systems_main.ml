open Bap_knowledge
open Core_kernel
open Bap_main
open Bap_primus.Std
open Format


let doc what =
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


let () = Extension.declare @@ fun ctxt ->
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
  let doc = doc name in
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
  make_info_command Primus.Components.list "component"
