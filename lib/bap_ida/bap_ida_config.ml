open Core_kernel.Std

let config_file = "~/.bap/config"

let config_read filename =
  let s_pair_of_sexp = pair_of_sexp string_of_sexp string_of_sexp in
  let s_pair_list_of_sexp = list_of_sexp s_pair_of_sexp in
  let config_pair = In_channel.with_file
      filename ~f:(fun ch -> Sexp.input_sexp ch
                             |> pair_of_sexp string_of_sexp
                               s_pair_list_of_sexp) in
  let alist = match config_pair with
    | ("BAP", x) -> x
    | _ -> assert false
  in
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
