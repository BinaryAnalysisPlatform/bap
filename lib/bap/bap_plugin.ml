open Core_kernel.Std
open Bap_types.Std
open Or_error

type 'a or_error = 'a Or_error.t

type t = {
  name : string;
  path : string;
  system : string;
} with fields

let init_findlib = lazy (
  Dynlink.init ();
  Dynlink.allow_unsafe_modules true;
  Findlib.init ())

let create_exn ~system name =
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
  else Some {
      name;
      path;
      system;
    }

let create ~system name =
  try create_exn ~system name with
  | Not_found -> None

let load pkg : unit or_error =
  try Ok (Dynlink.loadfile pkg.path) with
  | Dynlink.Error err ->
    error_string (Dynlink.error_message err)
  | exn -> of_exn exn

let list ~system : t list =
  let lazy () = init_findlib in
  Fl_package_base.list_packages () |>
  List.filter_map ~f:(create ~system)

let load_all ~system : (t * unit Or_error.t) list =
  list ~system |> List.map ~f:(fun pkg -> pkg, load pkg)
