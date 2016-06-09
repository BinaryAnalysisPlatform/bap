open Core_kernel.Std

let config_file =
  let (/) = Filename.concat in
  Sys.getenv "HOME" / ".bap" / "config"

type bap_dict =
  | BAP of string
  | KV of string*string

let bap_dict_of_sexp sexp = match sexp with
  | Sexp.Atom atom -> BAP atom
  | Sexp.List [Atom k; Atom v] -> KV (k,v)
  | Sexp.List _ -> Sexplib.Conv_error.no_variant_match "bap_dict" sexp

let stripkv kv_list =
  List.map kv_list ~f:(
    fun x -> match x with
      | BAP _ -> assert false
      | KV (k,v) -> (k,v)
  )

let config_read filename =
  let config_pair = In_channel.with_file
      filename ~f:(fun ch -> Sexp.input_sexp ch
                             |> list_of_sexp bap_dict_of_sexp) in
  let kv_list = match config_pair with
    | (BAP "BAP") :: x -> x
    | _ -> assert false
  in
  let alist = stripkv kv_list in
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
