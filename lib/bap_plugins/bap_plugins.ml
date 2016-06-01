open Core_kernel.Std
open Bap_bundle.Std
open Bap_future.Std
open Or_error.Monad_infix



module Plugin = struct

  type 'a or_error = 'a Or_error.t

  type t = {
    path : string;
    name : string;
    bundle : bundle sexp_opaque;
    loaded : unit future sexp_opaque;
    finish : unit promise sexp_opaque;
  } [@@deriving fields, sexp_of]




  type system_event = [
    | `Opening  of string
    | `Loading of t
    | `Linking of string
    | `Loaded  of t
    | `Errored of string * Error.t
  ] [@@deriving sexp_of]


  let system_events,event = Stream.create ()
  let notify (value : system_event) = Signal.send event value

  type reason = [
    | `In_core
    | `Provided_by of string
    | `Requested_by of string
  ]

  let load = ref Dynlink.loadfile
  let units : reason String.Table.t = String.Table.create ()

  let setup_dynamic_loader loader =
    load := loader

  let init = lazy begin
    Findlib.(recorded_packages Record_core) |> List.iter ~f:(fun pkg ->
        try
          let preds = Findlib.recorded_predicates () in
          let key = Findlib.package_property preds pkg "archive" |>
                    Filename.chop_extension in
          Hashtbl.set units ~key ~data:`In_core
        with exn -> ())
  end

  let of_path path =
    try
      notify (`Opening path);
      let bundle = Bundle.of_uri (Uri.of_string path) in
      let name = Bundle.manifest bundle |> Manifest.name in
      let loaded,finish = Future.create () in
      {name; path; bundle; loaded; finish}
    with exn ->
      let err = Error.of_exn exn in
      notify (`Errored (path,err));
      raise exn

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
      notify (`Linking pkg);
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
      Or_error.errorf "a name %S clashes with a %s" main with_whom

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

  let try_load (plugin : t) =
    if Future.is_decided plugin.loaded
    then Ok ()
    else
      let lazy () = init in
      notify (`Loading plugin);
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
      Promise.fulfill plugin.finish ();
      notify (`Loaded plugin);
      List.iter mains ~f:(fun unit ->
          Hashtbl.set units ~key:unit ~data:reason)

  let load plugin =
    match try_load plugin with
    | Error err as result ->
      notify (`Errored (plugin.name,err));
      result
    | result -> result

end

module Plugins = struct
  let paths_of_env () =
    try Sys.getenv "BAP_PLUGIN_PATH" |> String.split ~on:':'
    with Not_found -> []

  let plugin_paths library =
    [library; paths_of_env (); [Bap_config.libdir]] |> List.concat

  let collect ?(library=[]) () =
    let (/) = Filename.concat in
    plugin_paths library |> List.concat_map ~f:(fun dir ->
        Sys.readdir dir |>
        Array.to_list |>
        List.filter_map ~f:(fun file ->
            let file = dir / file in
            if Filename.check_suffix file ".plugin"
            then
              try Some (Ok (Plugin.of_path file))
              with exn -> Some (Error (file,Error.of_exn exn))
            else None))

  let list ?library () =
    collect ?library () |> List.filter_map ~f:(function
        | Ok p -> Some p
        | Error _ -> None)

  let load ?library ?(exclude=[]) () =
    collect ?library () |> List.filter_map ~f:(function
        | Error err -> Some (Error err)
        | Ok p when List.mem exclude (Plugin.name p) -> None
        | Ok p -> match Plugin.load p with
          | Ok () -> Some (Ok p)
          | Error err -> Some (Error (Plugin.path p, err)))

  let loaded,finished = Future.create ()

  let load ?library ?exclude () =
    if Future.is_decided loaded
    then []
    else begin
      let r = load ?library ?exclude () in
      Promise.fulfill finished ();
      r
    end

  (* we will record all events from the plugin subsystem,
     and if a bad thing happens, we will dump the full history.
     We will clean the history once all plugins are loaded, to prevent
     a memory leak.  *)
  let setup_default_handler () =
    let events_backtrace = ref [] in
    Stream.observe Plugin.system_events (fun ev -> match ev with
        | `Errored (name,err) ->
          let backtrace = String.concat ~sep:"\n" @@
            List.map !events_backtrace ~f:(fun ev ->
                Format.asprintf "%a" Sexp.pp
                  (Plugin.sexp_of_system_event ev)) in
          Format.eprintf "An error has occured while loading `%s': %a\n
                          Events backtrace:\n%s\n
                          Aborting program ...\n%!"
            name Error.pp err backtrace;
          exit 1
        | ev ->
          events_backtrace := ev :: !events_backtrace);
    Future.upon loaded (fun () -> events_backtrace := [])


  let run ?(don't_setup_handlers=false) ?library ?exclude () =
    if not don't_setup_handlers
    then setup_default_handler ();
    load ?library ?exclude () |> ignore

  let events = Plugin.system_events
  type event = Plugin.system_event [@@deriving sexp_of]

end

module Std = struct
  type plugin = Plugin.t
  module Plugin = Plugin
  module Plugins = Plugins
  let setup_dynamic_loader = Plugin.setup_dynamic_loader
  let list_loaded_units () = Hashtbl.keys Plugin.units
end
