open Core_kernel
open Bap_bundle.Std
open Bap_future.Std
open Or_error.Monad_infix

module Filename = Caml.Filename

module Plugin = struct

  type 'a or_error = 'a Or_error.t

  let args = ref Sys.argv

  let argv () = !args

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

  (* this function requires working Findlib infrastructure. *)
  let unit_of_package pkg =
    let preds = Findlib.recorded_predicates () in
    try
      Findlib.package_property preds pkg "archive" |>
      Filename.chop_extension
    with _ -> pkg  (* fails if the infrastructure is broken *)

  let init = lazy begin
    Findlib.(recorded_packages Record_core) |> List.iter ~f:(fun pkg ->
        Hashtbl.set units ~key:(unit_of_package pkg) ~data:`In_core);
    Findlib.recorded_predicates () |> List.iter ~f:(fun pred ->
        match String.chop_prefix pred ~prefix:"used_" with
        | None -> ()
        | Some lib -> Hashtbl.set units ~key:lib ~data:`In_core)
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
  let tags p = manifest p |> Manifest.tags
  let cons p = manifest p |> Manifest.cons

  let find_library_exn name =
    let dir = Findlib.package_directory name in
    let nat = if Dynlink.is_native then "native" else "byte" in
    let pre = ["plugin"; nat ] in
    let cmx = Findlib.package_property pre name "archive" in
    let file = Dynlink.adapt_filename cmx in
    Findlib.resolve_path ~base:dir file

  let find_library name =
    try Some (find_library_exn name) with _ -> None

  let load_unit ~reason ~name pkg : unit or_error =
    let open Format in
    try
      notify (`Linking name);
      !load pkg;
      Hashtbl.set units ~key:name ~data:reason;
      Ok ()
    with
    | Dynlink.Error err ->
      Or_error.error_string (Dynlink.error_message err)
    | exn -> Or_error.of_exn exn


  let is_debugging () =
    try String.(Sys.getenv "BAP_DEBUG" <> "0") with Caml.Not_found -> false

  let do_if_not_debugging f x =
    if is_debugging () then () else f x

  (** [load_entry plugin name] loads a compilation unit with the
      specified [name] required by [plugin]. The compilation unit
      should not be already linked to the main program. The unit is
      looked in the plugin bundle first and, if not found, looked in the
      system using the [Findlib] library. If the [findlib]
      infrastructure is not available (and a unit wasn't found in
      the bundle) returns an error, otherwise returns unit.

      The loaded unit is recorded in the internal registry of units,
      linked into the host program (unless the `don't_register` option
      is set to [true]).

      We track compilation units by their names, where the name
      consists of the base name of library with the extension
      chopped. This is a slightly better granularity, than tracking
      only the library names, but still not the ideal. Ideally, this
      should be tracked by the language runtime.

      Note: we need to track loaded units in order to prevent the
      linking of the same compilation unit twice, as if that happens a
      memory of a unit is corrupted (the global roots are
      overwritten). This is a known bug/issue in the OCaml runtime
      that we need to workaround.
  *)
  let load_entry plugin name =
    let suffix = if Dynlink.is_native then ".cmxs" else ".cma" in
    let name = Filename.basename name in
    let dst = Filename.(temp_file name suffix) in
    let path = Uri.of_string (name ^ suffix) in
    let reason = `Requested_by plugin.name in
    Bundle.get_file ~name:dst plugin.bundle path |> function
    | Some uri ->
      let path = Uri.to_string uri in
      let result = load_unit ~reason ~name path in
      do_if_not_debugging Sys.remove path;
      result
    | None -> match find_library name with
      | Some lib ->
        let name = Filename.(basename name |> chop_extension) in
        load_unit ~reason ~name lib
      | None -> Or_error.error_string "dependency not found"

  let validate_unit _plugin main =
    match Hashtbl.find units main with
    | None -> Ok ()
    | Some (`Provided_by name) when String.equal name main -> Ok ()
    | Some reason ->
      let with_whom = match reason with
        | `In_core -> "host program"
        | `Provided_by p -> "plugin " ^ p
        | `Requested_by p -> "library from plugin " ^ p in
      Or_error.errorf "a name %S clashes with a %s" main with_whom

  let validate_provided plugin mains =
    List.map mains ~f:(validate_unit plugin) |>
    Or_error.all_unit

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
      load_entry plugin main >>| fun () ->
      Promise.fulfill plugin.finish ();
      notify (`Loaded plugin);
      set_main_bundle old_bundle;
      let reason = `Provided_by plugin.name in
      List.iter mains ~f:(fun unit ->
          Hashtbl.set units ~key:unit ~data:reason)


  let with_argv argv f = match argv with
    | None -> f ()
    | Some argv ->
      let old = !args in
      args := argv;
      try
        let r = f () in
        args := old;
        r
      with
      | exn ->
        args := old;
        raise exn


  let do_load plugin =
    match try_load plugin with
    | Error err as result ->
      notify (`Errored (plugin.name,err));
      result
    | result -> result

  let load ?argv plugin =
    with_argv argv (fun () -> do_load plugin)

end

module Plugins = struct
  let paths_of_env () =
    try Sys.getenv "BAP_PLUGIN_PATH" |> String.split ~on:':'
    with Caml.Not_found -> []

  let path = Bap_plugins_config.plugindir

  let plugin_paths library =
    [library; paths_of_env (); [path]] |>
    List.concat |>
    List.filter ~f:Sys.file_exists |>
    List.filter ~f:Sys.is_directory

  let has_feature requested features =
    match requested with
    | None -> true
    | Some requested -> List.exists features ~f:(Set.mem requested)

  let meets_constraint provided requirements =
    List.for_all requirements ~f:(Set.mem provided)

  let is_selected ~provides ~env p =
    has_feature provides (Plugin.tags p) &&
    meets_constraint env (Plugin.cons p)


  let plugin_name = function
    | Ok p -> Plugin.name p
    | Error (name,_) -> name

  let collect ?env ?provides ?(library=[]) () =
    let (/) = Filename.concat in
    let strset = Set.of_list (module String) in
    let provides = Option.map provides ~f:strset in
    let env = strset (Option.value env ~default:[]) in
    plugin_paths library |>
    List.concat_map ~f:(fun dir ->
        Sys.readdir dir |> Array.to_list |>
        List.filter_map ~f:(fun file ->
            let file = dir / file in
            if not (Filename.check_suffix file ".plugin")
            then None
            else try
                let p = Plugin.of_path file in
                Option.some_if (is_selected ~provides ~env p) (Ok p)
              with exn -> Some (Error (file,Error.of_exn exn)))) |>
    List.sort ~compare:(fun x y ->
        String.compare (plugin_name x) (plugin_name y))


  let list ?env ?provides ?library () =
    collect ?env ?provides ?library () |> List.filter_map ~f:(function
        | Ok p -> Some p
        | Error _ -> None)

  let loaded,finished = Future.create ()

  let load ?argv ?env ?provides ?library ?(exclude=[]) () =
    if Future.is_decided loaded
    then []
    else begin
      Dynlink.allow_unsafe_modules true;
      let excluded = Set.of_list (module String) exclude in
      let r =
        collect ?env ?provides ?library () |>
        List.filter_map ~f:(function
            | Error err -> Some (Error err)
            | Ok p when Set.mem excluded (Plugin.name p) -> None
            | Ok p -> match Plugin.load ?argv p with
              | Ok () -> Some (Ok p)
              | Error err -> Some (Error (Plugin.path p, err))) in
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
          Format.eprintf "An error has occurred while loading `%s': %a\n
                          Events backtrace:\n%s\n
                          Aborting program ...\n%!"
            name Error.pp err backtrace;
          exit 1
        | ev ->
          events_backtrace := ev :: !events_backtrace);
    Future.upon loaded (fun () -> events_backtrace := [])


  let run ?argv ?env ?provides ?(don't_setup_handlers=false) ?library ?exclude () =
    if not don't_setup_handlers
    then setup_default_handler ();
    let _ : (Plugin.t, string * Error.t) result list =
      load ?argv ?env ?provides ?library ?exclude ()  in
    ()

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
