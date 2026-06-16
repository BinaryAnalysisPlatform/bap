open Base
open Stdio

open Stdlib.Scanf
module Sys = Stdlib.Sys
module Package = Bap_plugins_package
module Config = Bap_plugins_config
module Units = Bap_plugins_units.Make()
open Bap_plugins_loader_backend

let requires str =
  try Some (sscanf str "requires = %S" Fn.id) with _ -> None

let parse_meta path =
  In_channel.read_lines path |>
  List.find_map ~f:requires

let collect_plugins plugindir =
  if Sys.file_exists plugindir &&
     Sys.is_directory plugindir then
    Sys.readdir plugindir |>
    Array.to_list |>
    List.filter_map ~f:(fun path ->
        let meta = plugindir ^ "/" ^ path ^ "/" ^ "META" in
        if Sys.file_exists meta then parse_meta meta
        else None)
  else []


module Plugins = struct
  let paths = [Config.plugindir ^ "/plugins"]
  let list () = List.concat_map paths ~f:collect_plugins
  let load name =
    match Units.lookup name with
    | Some _ -> ()
    | None ->
      match Package.resolve name with
      | None -> invalid_arg ("unknown package " ^ name)
      | Some lib ->
        Units.record name (`Requested_by name);
        load lib
  let load_all () =
    list () |> List.iter ~f:load

end
