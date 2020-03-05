open Core_kernel
open Bap_main
open Bap_primus.Std
open Format

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


let () = Extension.Command.(declare "primus-systems" args) @@ fun _ctxt ->
  Primus.System.Repository.list () |>
  List.iter ~f:(Primus.Info.pp Format.std_formatter);
  Ok ()

let () = Extension.Command.(declare "primus-components" args) @@ fun _ctxt ->
  Primus.Components.list () |>
  List.iter ~f:(Primus.Info.pp Format.std_formatter);
  Ok ()
