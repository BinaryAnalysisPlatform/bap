open Core_kernel.Std
open Bap_plugins.Std
open Bap.Std
open Bap_cmdline_terms
open Cmdliner

type action = Keep | Drop

let filter_option options prefix =
  let is_long = String.is_prefix ~prefix:"--" in
  let is_our = String.is_prefix ~prefix in
  let is_key = String.is_prefix ~prefix:"-" in
  List.fold options ~init:([],Keep) ~f:(fun (opts,act) opt ->
      match act with
      | _ when prefix = opt -> (opts,Keep)
      | _ when is_our opt && is_long opt ->
        if String.mem opt '=' then (opts,Keep) else (opts,Drop)
      | _ when is_our opt -> (opts,Keep)
      | Drop when is_key opt && not(is_our opt) -> (opt::opts,Keep)
      | Keep -> (opt::opts,Keep)
      | Drop -> (opts,Keep)) |>
  fst |> List.rev

let filter_options ~known_passes ~argv =
  let opts = Array.to_list argv in
  let prefixes = List.map known_passes ~f:(fun pass -> "--"^pass) in
  List.fold ("-l" :: prefixes) ~init:opts ~f:filter_option |>
  Array.of_list



let run_and_get_passes argv =
  let library = fst (Cmdliner.Term.eval_peek_opts ~argv load_path) in
  let plugins = Option.value (fst (Term.eval_peek_opts ~argv load))
      ~default:[] in
  List.iter plugins ~f:(fun name ->
      Plugin.find_plugin ?library name |> function
      | None ->
        Error.raise @@ Error.of_string @@
        sprintf "failed to find plugin with name '%s'" name
      | Some p -> match Plugin.load p with
        | Ok () -> ()
        | Error err ->
          Error.raise @@ Error.of_string @@
          sprintf "failed to load plugin %s: %s"
            name (Error.to_string_hum err));
  let known_passes = match Project.passes ?library () with
    | Ok plugins -> plugins
    | Error err -> Error.raise err in
  let argv = filter_options ~known_passes ~argv:Sys.argv in
  let to_pass opt =
    List.find known_passes ~f:(fun plugin -> opt = "--"^plugin) in
  let passes = Array.(to_list @@ filter_map argv ~f:to_pass) in
  argv,passes


let run argv = fst (run_and_get_passes argv)
