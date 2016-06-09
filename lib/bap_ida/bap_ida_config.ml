open Core_kernel.Std

let config_file = "~/.bap/config"

let config_to_hashtbl filename =
  let sexp_to_hashtbl_str =
    Sexplib.Conv.hashtbl_of_sexp string_of_sexp string_of_sexp in
  let config_pair = In_channel.with_file filename ~f:(fun ch ->
      Sexp.input_sexp ch
      |> pair_of_sexp string_of_sexp sexp_to_hashtbl_str) in
  match config_pair with
  | ("BAP", hashtbl) -> hashtbl
  | _ -> assert false

let config_hashtbl = config_to_hashtbl config_file

let ida_path = match Hashtbl.find config_hashtbl "ida_path" with
  | Some path -> path
  | None -> assert false

let is_headless = false (* TODO Read this from config *)
