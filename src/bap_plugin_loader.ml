open Core_kernel.Std
open Bap_plugins.Std
open Bap_future.Std
open Bap.Std
open Bap_cmdline_terms
open Cmdliner
open Format

include Self()

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

let filter_options ~known_plugins ~known_passes ~argv =
  let opts = Array.to_list argv in
  let known_names = known_passes @ known_plugins in
  let prefixes = "--no" :: loader_options @
                 List.map known_names  ~f:(fun pass -> "--"^pass^"-") @
                 List.map known_passes ~f:(fun pass -> "--"^pass) in
  List.fold prefixes ~init:opts ~f:filter_option |> Array.of_list

let get_opt ~default  argv opt  =
  Option.value (fst (Term.eval_peek_opts ~argv opt)) ~default

let print_plugins_and_exit excluded plugins =
  List.iter plugins ~f:(fun p ->
      let status = if List.mem excluded (Plugin.name p)
        then "[-]" else "[+]" in
      printf "  %s %-16s %s@." status (Plugin.name p) (Plugin.desc p));
  exit 0

let exit_if_plugin_help_was_requested plugins argv =
  Array.exists argv ~f:(fun arg ->
      List.exists plugins ~f:(fun plugin ->
          "--"^plugin^"-help" = arg)) |> function
  | true -> exit 0
  | false -> ()

let excluded argv =
  let module Pat = String.Search_pattern in
  let pat = Pat.create "--no-" in
  Array.fold ~init:[] argv ~f:(fun plugins opt ->
      match Pat.index pat opt with
      | Some 0 -> Pat.replace_first pat opt "" :: plugins
      | _ -> plugins)

exception Plugin_not_found of string

let open_plugin_exn name =
  let name = name ^ ".plugin" in
  if Sys.file_exists name then Plugin.of_path name
  else raise (Plugin_not_found name)

let open_plugin ~verbose name =
  try
    let p = open_plugin_exn name in
    if verbose then
      eprintf "Loader: opened plugin %s\n" @@ Plugin.name p;
    Some p
  with
  | Plugin_not_found name ->
    eprintf "Loader: can't find plugin %s\n" name;
    None
  | exn when verbose ->
    eprintf "Loader: can't open plugin %s: %a\n"
      name Exn.pp exn;
    None
  | exn ->
    eprintf "Loader: can't open plugin %s\n" name; None

(** We will not ignore errors on plugins loaded explicitly
    by a user with `-l` option. *)
let load_plugin p = ok_exn (Plugin.load p)


let autoload_plugins ~library ~verbose ~exclude =
  Plugins.run ~library ~exclude ()
    ~don't_setup_handlers:true
(* we don't want to fail the whole platform if some
   plugin has failed, we will just emit an error message.  *)

let run_and_get_passes argv =
  let library = get_opt argv load_path ~default:[] in
  let verbose = get_opt argv verbose ~default:false in
  let plugins = get_opt argv load ~default:[] in
  let exclude = get_opt argv disable_plugin ~default:[] in
  let exclude = exclude @ excluded argv in
  let list = get_opt argv list_plugins ~default:false in
  let plugins = List.filter_map plugins ~f:(open_plugin ~verbose) in
  let known_plugins = Plugins.list ~library () @ plugins in
  if list then print_plugins_and_exit exclude known_plugins;
  List.iter plugins ~f:load_plugin;
  let noautoload = get_opt argv no_auto_load ~default:false in
  if not noautoload then autoload_plugins ~library ~verbose ~exclude;
  let known_passes = Project.passes () |>
                     List.map ~f:Project.Pass.name in
  let known_plugins = List.map known_plugins ~f:Plugin.name in
  let known_names = known_plugins @ known_passes in
  exit_if_plugin_help_was_requested known_names argv;
  let to_pass opt =
    List.find known_passes ~f:(fun plugin -> opt = "--"^plugin) in
  filter_options ~known_plugins ~known_passes ~argv:Sys.argv,
  Array.(to_list @@ filter_map argv ~f:to_pass)

let run argv = fst (run_and_get_passes argv)
