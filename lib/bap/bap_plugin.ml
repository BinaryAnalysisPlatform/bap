open Core_kernel.Std
open Bap_types.Std
open Or_error

type 'a or_error = 'a Or_error.t

type t = {path : string; name : string}
with bin_io,compare,fields,sexp

let system = "bap.plugin"

let of_path path = { path; name = Filename.basename path}

let init_findlib = lazy (
  try
    Dynlink.init ();
    Dynlink.allow_unsafe_modules true;
    Findlib.init ()
  with _ -> ())

let find_library_exn name =
  let module Pkg = Fl_package_base in
  let pkg = Pkg.query name in
  let def = Pkg.(pkg.package_defs) in
  let dir = Findlib.package_directory name in
  let nat = if Dynlink.is_native then "native" else "byte" in
  let pre = ["plugin"; nat ] in
  let cmx = Findlib.package_property pre name "archive" in
  let sys = Fl_metascanner.lookup "plugin_system" [] def in
  let file = Dynlink.adapt_filename cmx in
  let path = Findlib.resolve_path ~base:dir file in
  if sys <> system then None
  else Some {path; name}

let find_library name =
  try find_library_exn name with Not_found -> None

let load pkg : unit or_error =
  try Ok (Dynlink.loadfile pkg.path) with
  | Dynlink.Error err ->
    error_string (Dynlink.error_message err)
  | exn -> of_exn exn

let find_libraries () : t list =
  let lazy () = init_findlib in
  Fl_package_base.list_packages () |>
  List.filter_map ~f:find_library

let paths_of_env () =
  try Sys.getenv "BAP_PLUGIN_PATH" |> String.split ~on:':'
  with Not_found -> []

let undash = String.map ~f:(function '-' -> '_' | c -> c)

let normalize_path name =
  if Filename.check_suffix name ".plugin"
  then name else name ^ ".plugin"

let normalize_name name =
  let name = Filename.basename name in
  if Filename.check_suffix name ".plugin"
  then Filename.chop_extension name else name

let find_plugin ?(library=[]) name =
  let filename = normalize_path name in
  let name = normalize_name name in
  let paths =
    [[FileUtil.pwd ()]; paths_of_env (); library] |> List.concat in
  List.find_map paths ~f:(fun dir ->
      let path = Filename.concat dir filename in
      if Sys.file_exists path then Some {path; name} else
      if Sys.file_exists (undash path)
      then Some {path=(undash path); name}
      else None)
