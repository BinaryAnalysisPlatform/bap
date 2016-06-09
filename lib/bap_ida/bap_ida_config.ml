open Core_kernel.Std

let config_file =
  let (/) = Filename.concat in
  Sys.getenv "HOME" / ".bap" / "config"

let config_alist_of_sexp sexp =
  let kv_of_sexp = pair_of_sexp string_of_sexp string_of_sexp in
  let open Sexp in
  match sexp with
  | List ((Atom "BAP")::kv_list) -> List.map ~f:kv_of_sexp kv_list
  | Atom _ -> of_sexp_error "config must be a list" sexp
  | List _ -> of_sexp_error "config must start with \"(BAP\"" sexp

let config_read filename =
  try
    let alist = In_channel.with_file
        filename ~f:(fun ch -> Sexp.input_sexp ch
                               |> config_alist_of_sexp)
    in
    match Map.of_alist String.comparator alist with
    | `Ok map -> Some map
    | _ -> None
  with Sys_error _ -> None


let config = config_read config_file

let get_config key =
  match config with
  | Some m -> Map.find m key
  | None -> None

let config_exists key =
  match get_config key with
  | Some _ -> true
  | None -> false

let ida_path : string = match get_config "ida_path" with
  | Some path -> path
  | None -> ""

let is_headless : bool = match get_config "is_headless" with
  | Some ("True"|"true") -> true
  | Some _ -> false
  | None -> false

let is_plugin_loadable : bool = config_exists "ida_path"
