open Core_kernel.Std

let config_file =
  let (/) = Filename.concat in
  Sys.getenv "HOME" / ".bap" / "config"

exception Sexp_Error_BAP_Dict

let config_alist_of_sexp sexp =
  let kv_of_sexp = pair_of_sexp string_of_sexp string_of_sexp in
  let alist_of_sexp = list_of_sexp kv_of_sexp in
  match sexp with
  | Sexp.List ((Sexp.Atom "BAP")::kv_list) -> alist_of_sexp sexp
  | Sexp.Atom _ -> raise Sexp_Error_BAP_Dict
  | Sexp.List _ -> raise Sexp_Error_BAP_Dict

let config_read filename =
  let alist = In_channel.with_file
      filename ~f:(fun ch -> Sexp.input_sexp ch|> config_alist_of_sexp) in
  match Map.of_alist String.comparator alist with
  | `Ok map -> map
  | _ -> assert false

let config = config_read config_file

let get_config key =
  match Map.find config key with
  | Some v -> v
  | None -> assert false

let ida_path = get_config "ida_path"

let is_headless = false (* TODO Read this from config *)
