open Core_kernel.Std
open Bap_bundle.Std
open Or_error.Monad_infix


module Plugin = struct

  type 'a or_error = 'a Or_error.t

  type t = {bundle : bundle; path : string; name : string} with fields

  type reason = [
    | `In_core
    | `Provided_by of string
    | `Requested_by of string
  ]

  let load = ref Dynlink.loadfile
  let units : reason String.Table.t = String.Table.create ()

  let setup_dynamic_loader loader =
    load := loader

  let () =
    Findlib.(recorded_packages Record_core) |> List.iter ~f:(fun pkg ->
        try
          let preds = Findlib.recorded_predicates () in
          let key = Findlib.package_property preds pkg "archive" |>
                    Filename.chop_extension in
          Hashtbl.set units ~key ~data:`In_core
        with exn -> ())

  let of_path path =
    let bundle = Bundle.of_uri (Uri.of_string path) in
    {
      name = Bundle.manifest bundle |> Manifest.name;
      path; bundle
    }

  let manifest p = Bundle.manifest p.bundle
  let name p = manifest p |> Manifest.name
  let desc p = manifest p |> Manifest.desc

  let find_library_exn name =
    let dir = Findlib.package_directory name in
    let nat = if Dynlink.is_native then "native" else "byte" in
    let pre = ["plugin"; nat ] in
    let cmx = Findlib.package_property pre name "archive" in
    let file = Dynlink.adapt_filename cmx in
    Findlib.resolve_path ~base:dir file

  let find_library name =
    try Some (find_library_exn name) with Not_found -> None

  let load_unit ~reason ~name pkg : unit or_error =
    let open Format in
    try
      !load pkg;
      Hashtbl.set units ~key:name ~data:reason;
      Ok ()
    with
    | Dynlink.Error err ->
      Or_error.error_string (Dynlink.error_message err)
    | exn -> Or_error.of_exn exn

  let load_entry ?(don't_register=false) plugin name =
    let suffix = if Dynlink.is_native then ".cmxs" else ".cma" in
    let name = Filename.basename name in
    let dst = Filename.(temp_file name suffix) in
    let path = Uri.of_string (name ^ suffix) in
    let reason = `Requested_by plugin.name in
    Bundle.get_file ~name:dst plugin.bundle path |> function
    | Some uri ->
      let path = Uri.to_string uri in
      let result = load_unit ~reason ~name path in
      Sys.remove path;
      result
    | None -> match find_library name with
      | Some lib ->
        let name = Filename.(basename name |> chop_extension) in
        load_unit ~reason ~name lib
      | None -> Or_error.error_string "dependency not found"

  let validate_unit plugin main =
    match Hashtbl.find units main with
    | None -> Ok ()
    | Some (`Provided_by name) when name = main -> Ok ()
    | Some reason ->
      let with_whom = match reason with
        | `In_core -> "host program"
        | `Provided_by p -> "plugin " ^ p
        | `Requested_by p -> "library from plugin " ^ p in
      Or_error.errorf "a name %s clashes with a %s" main with_whom

  let validate_provided plugin mains =
    List.map mains ~f:(validate_unit plugin) |>
    Or_error.all_ignore

  let load_entries plugin entries =
    List.fold entries ~init:(Ok ()) ~f:(fun so_far entry ->
        so_far >>= fun () ->
        match load_entry plugin entry with
        | Ok () -> Ok ()
        | Error err ->
          Or_error.errorf "Failed to load %s: %s" entry @@
          Error.to_string_hum err)

  let load plugin =
    let m = manifest plugin in
    let mains = Manifest.provides m in
    validate_provided plugin mains >>= fun () ->
    let reqs = Manifest.requires m |>
               List.filter ~f:(fun r -> not (Hashtbl.mem units r)) in
    let main = Manifest.main m in
    let old_bundle = main_bundle () in
    set_main_bundle (bundle plugin);
    load_entries plugin reqs >>= fun () ->
    load_entry ~don't_register:true plugin main >>| fun () ->
    let reason = `Provided_by plugin.name in
    set_main_bundle old_bundle;
    List.iter mains ~f:(fun unit ->
        Hashtbl.set units ~key:unit ~data:reason)
end

module Plugins = struct
  let paths_of_env () =
    try Sys.getenv "BAP_PLUGIN_PATH" |> String.split ~on:':'
    with Not_found -> []

  let plugin_paths library =
    [library; paths_of_env (); [Bap_config.libdir]] |> List.concat

  let list ?(library=[]) () =
    let (/) = Filename.concat in
    plugin_paths library |> List.concat_map ~f:(fun dir ->
        Sys.readdir dir |>
        Array.to_list |>
        List.filter_map ~f:(fun file ->
            let file = dir / file in
            if Filename.check_suffix file ".plugin"
            then Some (Plugin.of_path file)
            else None))

  let load ?library ?(exclude=[]) () =
    list ?library () |> List.filter_map ~f:(fun p ->
        if Plugin.name p |> List.mem exclude
        then None
        else Some (p, Plugin.load p))
end

module Std = struct
  type plugin = Plugin.t
  module Plugin = Plugin
  module Plugins = Plugins
  let setup_dynamic_loader = Plugin.setup_dynamic_loader
  let list_loaded_units () = Hashtbl.keys Plugin.units
end
